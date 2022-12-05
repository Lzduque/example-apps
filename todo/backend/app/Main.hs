module Main where

-- import qualified MyLib as Lib
import qualified Messages as Msg
import qualified Database as Db
import qualified Api.Types.RTodoListItem as RTodoListItem
import qualified Api.Types.CTodoListItem as CTodoListItem
import qualified Api.Types.UTodoListItem as UTodoListItem
import qualified Api.Types.CUser as CUser
import qualified Api.Types.RSession as RSession

import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Monad as M
import qualified Control.Concurrent as Conc
import qualified Data.Char as Char
import qualified Control.Exception as Except
import qualified Data.Maybe as Maybe
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson
import qualified Data.Proxy as Proxy
import Data.String.Conversions (cs)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

type WSId = UUID.UUID
type Client = (WSId, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
clientExists (client, _) serverState = any (\(x,_) -> x == client) serverState

addClient :: Client -> ServerState -> ServerState
addClient client clients
  | not (clientExists client clients) = client : clients
  | otherwise = clients

removeClient :: Client -> ServerState -> ServerState
removeClient client = filter ((/= fst client) . fst)

broadcast :: T.Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn $ "broadcasting: " <> message
  M.forM_ clients $ do
    \(_, conn) -> WS.sendTextData conn message

application :: Conc.MVar ServerState -> WS.ServerApp
application state pending = do
  putStrLn "application"
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg :: T.Text <- WS.receiveData conn
    T.putStrLn $ "Message received: " <> msg
    -- clients <- Conc.readMVar state
    handleConnectionMessage msg conn state
    
handleConnectionMessage
  :: T.Text -- ^ Message
  -> WS.Connection 
  -> Conc.MVar ServerState 
  -> IO ()
handleConnectionMessage msg conn state = do
  let reqConnectionMessage :: Maybe Msg.ReqConnection = Aeson.decode . cs $ msg
  if
    -- Invitation to listen to messages from a specific client
    | Maybe.isJust reqConnectionMessage -> do
      wsId <- UUID.nextRandom
      let client = (wsId, conn)
      handleNewConnection client state
    | otherwise -> do
      T.putStrLn $ "Message not recognized (connection): " <> msg

handleNewConnection
  :: Client
  -> Conc.MVar ServerState 
  -> IO ()
handleNewConnection client state = do
  clients <- Conc.readMVar state
  if
    | clientExists client clients -> do
      T.putStrLn $ "User already exists: " <> UUID.toText (fst client)
    | otherwise -> do
      flip Except.finally (disconnect client state) $ do
        Conc.modifyMVar_ state $ \s -> do
          let s' = addClient client s
          -- broadcast (fst client <> " joined") s'
          return s'
        let conn = snd client
        sendMessage conn Msg.ResConnection { type_ = Proxy.Proxy }
        connect client state

disconnect :: Client -> Conc.MVar ServerState -> IO ()
disconnect client state = do
  -- Remove client and return new state
  s <- Conc.modifyMVar state $ \s -> do
    let s' = removeClient client s
    return (s', s')
  print "SOMEONE DISCONNECTED"
  broadcast (UUID.toText (fst client) <> " disconnected") s

-- This is to continuously listen for messages from a specific client
connect :: Client -> Conc.MVar ServerState -> IO ()
connect (wsId, conn) state = M.forever $ do
  msg <- WS.receiveData conn
  handleClientMessage msg (wsId, conn) state

handleClientMessage :: T.Text -> Client -> Conc.MVar ServerState -> IO ()
handleClientMessage msg (wsId, conn) state = do
  -- DEBUGGING
  T.putStrLn $ "New msg: " <> msg
  T.putStrLn $ "From client: " <> UUID.toText wsId

  let reqRegister :: Maybe Msg.ReqRegister = Aeson.decode . cs $ msg
  let reqSignIn :: Maybe Msg.ReqSignIn = Aeson.decode . cs $ msg
  let reqTodoList :: Maybe Msg.ReqTodoList = Aeson.decode . cs $ msg
  let reqCreateTodo :: Maybe Msg.ReqCreateTodo = Aeson.decode . cs $ msg
  let reqDeleteTodo :: Maybe Msg.ReqDeleteTodo = Aeson.decode . cs $ msg
  let reqToggleTodo :: Maybe Msg.ReqToggleTodo = Aeson.decode . cs $ msg

  if
    | Maybe.isJust reqRegister -> do
      let email = T.toLower (Msg.reqRegisterEmail (Maybe.fromJust reqRegister))
      let password = Msg.reqRegisterPassword (Maybe.fromJust reqRegister)
      -- validate that user does not exist
      mUser <- Db.findUserByEmail email
      case mUser of 
        Just user -> do
          print "Error, user already registered" -- TEMP
          -- sendMessage conn Msg.ResRegister { type_ = Proxy.Proxy, error = RegisterError.EmailExists }
        Nothing -> do
          Db.createUser CUser.CUser
            { email = email
            , password = password
            }
          -- (also create and send session, for convenience)
          sendMessage conn Msg.ResRegister { type_ = Proxy.Proxy }
    | Maybe.isJust reqSignIn -> do
      let email = T.toLower (Msg.reqSignInEmail (Maybe.fromJust reqSignIn))
      let password = Msg.reqSignInPassword (Maybe.fromJust reqSignIn)
      -- validate auth
      -- should this be a new type? AuthUser { email, password }
      mUser <- Db.authenticateUser email password
      print $ "mUser: " ++ (show mUser)
      case mUser of
        Nothing -> do
          print "Auth failed, no user found" -- TEMP
          -- TODO: send error message
        Just user -> do -- auth succeeded
          -- generate session
          mSession <- Db.createSession user
          print $ "mSession: " ++ (show mSession)
          case mSession of
            Nothing -> do
              print "Auth failed, couldn't create session" -- TEMP
            Just session -> do
              sendMessage conn Msg.ResSignIn { type_ = Proxy.Proxy, sessionId = RSession.id session }
    | Maybe.isJust reqTodoList -> do
      sendTodoList (wsId, conn) state
    | Maybe.isJust reqCreateTodo -> do
      let name = (Msg.name :: Msg.ReqCreateTodo -> T.Text) (Maybe.fromJust reqCreateTodo)
      let sessionId = (Msg.reqCreateTodoSessionId :: Msg.ReqCreateTodo -> T.Text) (Maybe.fromJust reqCreateTodo)
      mSession <- Db.findSessionById sessionId
      case mSession of
        Nothing -> do
          print "Auth failed, couldn't retrieve session" -- TEMP
          -- TODO: send error message
        Just session -> do
          let userId = RSession.userId session
          Db.createTodo CTodoListItem.CTodoListItem
            { name = name
            , userId = userId
            }
          sendMessage conn Msg.ResCreateTodo { type_ = Proxy.Proxy }
    | Maybe.isJust reqDeleteTodo -> do
      Db.deleteTodo $ Msg.reqDeleteTodoId (Maybe.fromJust reqDeleteTodo)
      sendMessage conn Msg.ResDeleteTodo { type_ = Proxy.Proxy }
    | Maybe.isJust reqToggleTodo -> do
      let itemId = Msg.reqToggleTodoId (Maybe.fromJust reqToggleTodo)
      let checked = Msg.checked (Maybe.fromJust reqToggleTodo)
      Db.updateTodo itemId UTodoListItem.UTodoListItem
        { checked = checked
        }
      sendMessage conn Msg.ResToggleTodo { type_ = Proxy.Proxy }
    | otherwise -> do
      T.putStrLn $ "Message not recognized (user): " <> msg

sendTodoList :: Client -> Conc.MVar ServerState -> IO ()
sendTodoList (wsId, conn) state = do
  items <- Db.getTodoList
  let msg = Msg.ResTodoList { type_ = Proxy.Proxy, items = items }
  sendMessage conn msg

-- Should "Show a" be something more like "Message a"? To say that 'a' has to be a message, not just any string
sendMessage :: Aeson.ToJSON a => WS.Connection -> a -> IO ()
sendMessage conn msg = do
  WS.sendTextData conn (cs . Aeson.encode $ msg :: T.Text)

main :: IO ()
main = do
  state <- Conc.newMVar newServerState
  let address :: String = "127.0.0.1"
  let port :: Int = 9160
  putStrLn $
    "Listening on " <> address <> ":" <> show port
  -- let m = Msg.ReqConnection {userId = "1234"}
  -- putStrLn $ cs $ Aeson.encode m
  WS.runServer address port $ application state
