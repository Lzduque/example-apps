module Main where

-- import qualified MyLib as Lib
import qualified Messages as Msg
import qualified Database as Db
import qualified Api.Types.RTodoListItem as RTodoListItem
import qualified Api.Types.CTodoListItem as CTodoListItem
import qualified Api.Types.UTodoListItem as UTodoListItem
import qualified Api.Types.CUser as CUser
import qualified Api.Types.RUser as RUser
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
import qualified Data.List as L

type WSClientId = UUID.UUID
type UserId = Maybe Integer
data WSClient = WSClient
  { wsClientId :: WSClientId
  , userId :: UserId
  , conn :: WS.Connection
  }
instance Show WSClient where
  show client = "wsClientId: " ++ show (wsClientId client) ++ ", userId: " ++ show (userId client)
type ServerState = [WSClient]

newServerState :: ServerState
newServerState = []

numWSClients :: ServerState -> Int
numWSClients = length

clientExists :: WSClient -> ServerState -> Bool
clientExists client serverState = any (\x -> wsClientId x == wsClientId client) serverState

addWSClient :: WSClient -> ServerState -> ServerState
addWSClient client clients
  | not (clientExists client clients) = client : clients
  | otherwise = clients

updateWSClient :: WSClient -> ServerState -> ServerState
updateWSClient client clients = client : filter (\x -> wsClientId x /= wsClientId client) clients

removeWSClient :: WSClientId -> ServerState -> ServerState
removeWSClient clientId = filter ((/= clientId) . wsClientId)

getWSClient :: WSClientId -> ServerState -> Maybe WSClient
getWSClient clientId clients = L.find (\x -> wsClientId x == clientId) clients

broadcast :: T.Text -> ServerState -> IO ()
broadcast message clients = do
  T.putStrLn $ "broadcasting: " <> message
  M.forM_ clients $ do
    \client -> WS.sendTextData (conn client) message

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
      wsClientId <- UUID.nextRandom
      let client = WSClient 
            { wsClientId = wsClientId
            , userId = Nothing
            , conn = conn
            }
      handleNewConnection client state
    | otherwise -> do
      T.putStrLn $ "Message not recognized (connection): " <> msg

handleNewConnection
  :: WSClient
  -> Conc.MVar ServerState 
  -> IO ()
handleNewConnection client state = do
  clients <- Conc.readMVar state
  if
    | clientExists client clients -> do
      T.putStrLn $ "User already exists: " <> UUID.toText (wsClientId client)
    | otherwise -> do
      flip Except.finally (disconnect (wsClientId client) state) $ do
        T.putStrLn $ "[handleNewConnection] From client: " <> UUID.toText (wsClientId client)
        T.putStrLn $ "[handleNewConnection] From client: " <> cs (show (userId client))
        Conc.modifyMVar_ state $ \s -> do
          let s' = addWSClient client s
          -- broadcast (fst client <> " joined") s'
          return s'
        sendMessage (conn client) Msg.ResConnection { type_ = Proxy.Proxy }
        connect (wsClientId client) state

disconnect :: WSClientId -> Conc.MVar ServerState -> IO ()
disconnect clientId state = do
  -- Remove client and return new state
  s <- Conc.modifyMVar state $ \s -> do
    let s' = removeWSClient clientId s
    return (s', s')
  print "SOMEONE DISCONNECTED"
  -- broadcast (UUID.toText clientId <> " disconnected") s

-- This is to continuously listen for messages from a specific client
connect :: WSClientId -> Conc.MVar ServerState -> IO ()
connect clientId state = M.forever $ do
  T.putStrLn $ "[connect] From client: " <> UUID.toText clientId
  clients <- Conc.readMVar state
  T.putStrLn $ "[connect] clients: " <> cs (show clients)
  -- Get the client from the state (match by wsClientId) so we always have its latest version, with any modifications applied to state
  let mWSClient :: Maybe WSClient = getWSClient clientId clients
  case mWSClient of
    Nothing -> return ()
    Just client' -> do
      T.putStrLn $ "[connect] From client: " <> UUID.toText (wsClientId client')
      T.putStrLn $ "[connect] From client: " <> cs (show (userId client'))
      msg <- WS.receiveData (conn client')
      handleWSClientMessage msg (wsClientId client') state

handleWSClientMessage :: T.Text -> WSClientId -> Conc.MVar ServerState -> IO ()
handleWSClientMessage msg clientId state = do
  clients <- Conc.readMVar state
  let mClient = getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  
  -- DEBUGGING
  T.putStrLn $ "[handleWSClientMessage] New msg: " <> msg
  T.putStrLn $ "[handleWSClientMessage] From client: " <> UUID.toText (wsClientId client)
  T.putStrLn $ "[handleWSClientMessage] From client: " <> cs (show (userId client))

  let reqRegister :: Maybe Msg.ReqRegister = Aeson.decode . cs $ msg
  let reqSignIn :: Maybe Msg.ReqSignIn = Aeson.decode . cs $ msg
  let reqSignOut :: Maybe Msg.ReqSignOut = Aeson.decode . cs $ msg
  let reqTodoList :: Maybe Msg.ReqTodoList = Aeson.decode . cs $ msg
  let reqCreateTodo :: Maybe Msg.ReqCreateTodo = Aeson.decode . cs $ msg
  let reqDeleteTodo :: Maybe Msg.ReqDeleteTodo = Aeson.decode . cs $ msg
  let reqToggleTodo :: Maybe Msg.ReqToggleTodo = Aeson.decode . cs $ msg

  if
    | Maybe.isJust reqRegister -> do
      handleReqRegister (Maybe.fromJust reqRegister) clientId state
    | Maybe.isJust reqSignIn -> do
      handleReqSignIn (Maybe.fromJust reqSignIn) clientId state
    | Maybe.isJust reqSignOut -> do
      handleReqSignOut (Maybe.fromJust reqSignOut) clientId state
    | Maybe.isJust reqTodoList -> do
      handleReqTodoList (Maybe.fromJust reqTodoList) clientId state
    | Maybe.isJust reqCreateTodo -> do
      handleReqCreateTodo (Maybe.fromJust reqCreateTodo) clientId state
    | Maybe.isJust reqDeleteTodo -> do
      handleReqDeleteTodo (Maybe.fromJust reqDeleteTodo) clientId state
    | Maybe.isJust reqToggleTodo -> do
      handleReqToggleTodo (Maybe.fromJust reqToggleTodo) clientId state
    | otherwise -> do
      T.putStrLn $ "Message not recognized (user): " <> msg

handleReqRegister :: Msg.ReqRegister -> WSClientId -> Conc.MVar ServerState -> IO ()
handleReqRegister reqRegister clientId state = do
  clients <- Conc.readMVar state
  let mClient = getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient

  let email = T.toLower (Msg.reqRegisterEmail reqRegister)
  let password = Msg.reqRegisterPassword reqRegister
  -- validate that user does not exist
  -- if we wanted to after this we would check for lenght of password, characters, validity of email, etc and then react with errors if necessary
  mUser <- Db.findUserByEmail email
  case mUser of 
    Just user -> do
      sendMessage (conn client) Msg.ErrorRegisterEmail { type_ = Proxy.Proxy, text = "User already registered"}
      print "Error, user already registered" -- TEMP
    Nothing -> do
      mUser <- Db.createUser CUser.CUser
        { email = email
        , password = password
        }
      case mUser of
        Nothing -> do
          sendMessage (conn client) Msg.ErrorRegisterEmail { type_ = Proxy.Proxy, text = "Something went wrong :("}
          print "Auth failed, couldn't create user" -- TEMP
        Just user -> do
          -- (also create and send session, for convenience)
          mSession <- Db.createSession user
          case mSession of
            Nothing -> do
              sendMessage (conn client) Msg.ErrorRegisterEmail { type_ = Proxy.Proxy, text = "Something went wrong :("}
              print "Auth failed, couldn't create session" -- TEMP
            Just session -> do
              -- update the client in state to have the user ID
              let newClient = client { userId = Just (RUser.id user) }
              Conc.modifyMVar_ state $ \s -> do
                let s' = updateWSClient newClient s
                return s'
              sendMessage (conn client) Msg.ResRegister { type_ = Proxy.Proxy, resRegisterSessionId = RSession.id session  }

handleReqSignIn :: Msg.ReqSignIn -> WSClientId -> Conc.MVar ServerState -> IO ()
handleReqSignIn reqSignIn clientId state = do
  clients <- Conc.readMVar state
  let mClient = getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  let email = T.toLower (Msg.reqSignInEmail reqSignIn)
  let password = Msg.reqSignInPassword reqSignIn
  -- validate auth
  -- should this be a new type? AuthUser { email, password }
  mUser <- Db.authenticateUser email password
  case mUser of
    Nothing -> do
      print "Auth failed, no user found" -- TEMP
      -- TODO: send error message
      -- sendMessage conn Msg.ResSignIn { type_ = Proxy.Proxy, error = SignInError.Generic }
      -- sendMessage conn Msg.ResError { type_ = Proxy.Proxy, error = SignInError.Generic }
    Just user -> do -- auth succeeded
      -- generate session
      mSession <- Db.createSession user
      case mSession of
        Nothing -> do
          print "Auth failed, couldn't create session" -- TEMP
        Just session -> do
          -- update the client in state to have the user ID
          let newClient = client { userId = Just (RUser.id user) }
          Conc.modifyMVar_ state $ \s -> do
            let s' = updateWSClient newClient s
            return s'
          sendMessage (conn client) Msg.ResSignIn { type_ = Proxy.Proxy, resSignInSessionId = RSession.id session }

handleReqSignOut :: Msg.ReqSignOut -> WSClientId -> Conc.MVar ServerState -> IO ()
handleReqSignOut reqSignOut clientId state = do
  clients <- Conc.readMVar state
  let mClient = getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  let sessionId = Msg.reqSignOutSessionId reqSignOut
  Db.deleteSession sessionId
  sendMessage (conn client) Msg.ResSignOut { type_ = Proxy.Proxy }

handleReqTodoList :: Msg.ReqTodoList -> WSClientId -> Conc.MVar ServerState -> IO ()
handleReqTodoList reqTodoList clientId state = do
  clients <- Conc.readMVar state
  let mClient = getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  -- get user to be able to query only user's todos
  let mSessionId = Msg.reqTodoListSessionId reqTodoList
  case mSessionId of
    Nothing -> do
      print "Invalid session"
      sendMessage (conn client) Msg.ResSignOut { type_ = Proxy.Proxy }
    Just sessionId -> do
      mUserId <- Db.findUserIdBySessionId sessionId
      case mUserId of
        Nothing -> do
          print "Invalid session"
          Db.deleteSession sessionId
          sendMessage (conn client) Msg.ResSignOut { type_ = Proxy.Proxy }
        Just userId -> do
          let newWSClient = client { userId = Just userId }
          Conc.modifyMVar_ state $ \s -> do
            let s' = updateWSClient newWSClient s
            return s'
          sendTodoList (wsClientId newWSClient) state

handleReqCreateTodo :: Msg.ReqCreateTodo -> WSClientId -> Conc.MVar ServerState -> IO ()
handleReqCreateTodo reqCreateTodo clientId state = do
  clients <- Conc.readMVar state
  let mClient = getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  let name = (Msg.name :: Msg.ReqCreateTodo -> T.Text) reqCreateTodo
  let sessionId = (Msg.reqCreateTodoSessionId :: Msg.ReqCreateTodo -> T.Text) reqCreateTodo
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
      sendMessage (conn client) Msg.ResCreateTodo { type_ = Proxy.Proxy }

handleReqDeleteTodo :: Msg.ReqDeleteTodo -> WSClientId -> Conc.MVar ServerState -> IO ()
handleReqDeleteTodo reqDeleteTodo clientId state = do
  clients <- Conc.readMVar state
  let mClient = getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  Db.deleteTodo $ Msg.reqDeleteTodoId reqDeleteTodo
  sendMessage (conn client) Msg.ResDeleteTodo { type_ = Proxy.Proxy }

handleReqToggleTodo :: Msg.ReqToggleTodo -> WSClientId -> Conc.MVar ServerState -> IO ()
handleReqToggleTodo reqToggleTodo clientId state = do
  clients <- Conc.readMVar state
  let mClient = getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  let itemId = Msg.reqToggleTodoId reqToggleTodo
  let checked = Msg.checked reqToggleTodo
  Db.updateTodo itemId UTodoListItem.UTodoListItem
    { checked = checked
    }
  sendMessage (conn client) Msg.ResToggleTodo { type_ = Proxy.Proxy }

sendTodoList :: WSClientId -> Conc.MVar ServerState -> IO ()
sendTodoList clientId state = do
  clients <- Conc.readMVar state
  let mClient = getWSClient clientId clients

  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[sendTodoList] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient

  items <- case userId client of
    Nothing -> return []
    Just userId' -> Db.getTodoList userId'

  let msg = Msg.ResTodoList { type_ = Proxy.Proxy, items = items }
  sendMessage (conn client) msg

  let updateMsg = Msg.UpdateTodoList { type_ = Proxy.Proxy, items = items }
  broadcastUserMessage state client updateMsg

-- Should "Show a" be something more like "Message a"? To say that 'a' has to be a message, not just any string
sendMessage :: Aeson.ToJSON a => WS.Connection -> a -> IO ()
sendMessage conn msg = do
  WS.sendTextData conn (cs . Aeson.encode $ msg :: T.Text)

-- Send message to every other session that the client is logged in as (NOT including the client's current session)
broadcastUserMessage :: Aeson.ToJSON a => Conc.MVar ServerState -> WSClient -> a -> IO ()
broadcastUserMessage state client msg = do
  clients <- Conc.readMVar state
  let clients' = filter (\client' -> userId client' == userId client && not (wsClientId client' == wsClientId client)) clients
  M.forM_ clients' $ do
    \client' -> WS.sendTextData (conn client') (cs . Aeson.encode $ msg :: T.Text)

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
