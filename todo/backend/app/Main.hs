module Main where

-- import qualified MyLib as Lib
import qualified Messages as Msg

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
import Data.String.Conversions (cs)

type UserId = T.Text
type Client = (UserId, WS.Connection)
type ServerState = [Client]

-- data TodoListItems = TodoListItems {

-- }

-- {
-- 		type: 'list-with-items',
-- 		items: [
-- 			{id: 'item1', item: 'do the laundry'},
-- 			{id: 'item2', item: 'grocery shopping'},
-- 			{id: 'item3', item: "clean Maya's littler box every day"},
-- 		],
-- 	}

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
    clients <- Conc.readMVar state
    handleMessage msg conn state
    
handleMessage
  :: T.Text -- ^ Message
  -> WS.Connection 
  -> Conc.MVar ServerState 
  -> IO ()
handleMessage msg conn state = do
  let inConnectionMessage :: Maybe Msg.InConnection = Aeson.decode . cs $ msg
  if
    | Maybe.isJust inConnectionMessage -> do
      let userId' = Msg.userId (Maybe.fromJust inConnectionMessage)
      let client = (userId', conn)
      handleNewConnection client state
    | otherwise -> do
      T.putStrLn $ "Message not recognized: " <> msg

handleNewConnection
  :: Client
  -> Conc.MVar ServerState 
  -> IO ()
handleNewConnection client state = do
  clients <- Conc.readMVar state
  if
    | clientExists client clients -> do
      T.putStrLn $ "User already exists: " <> fst client
    | otherwise -> flip Except.finally (disconnect client state) $ do
      Conc.modifyMVar_ state $ \s -> do
        let s' = addClient client s
        -- broadcast (fst client <> " joined") s'
        return s'
      connect client state

disconnect :: Client -> Conc.MVar ServerState -> IO ()
disconnect client state = do
  -- Remove client and return new state
  s <- Conc.modifyMVar state $ \s -> do
    let s' = removeClient client s
    return (s', s')
  broadcast (fst client <> " disconnected") s

connect :: Client -> Conc.MVar ServerState -> IO ()
connect (user, conn) state = M.forever $ do
  msg <- WS.receiveData conn

  let inTodoListMessage :: Maybe Msg.InTodoList = Aeson.decode . cs $ msg

  -- if
  --   | Maybe.isJust inTodoListMessage -> do
  --     sendTodoList client state
    
  T.putStrLn $ "New msg: " <> msg
  T.putStrLn $ "From client: " <> user
  -- Conc.readMVar state >>= broadcast
  --   (user `mappend` ": " `mappend` msg)


main :: IO ()
main = do
  state <- Conc.newMVar newServerState
  let address :: String = "127.0.0.1"
  let port :: Int = 9160
  putStrLn $
    "Listening on " <> address <> ":" <> show port
  -- let m = Msg.InConnection {userId = "1234"}
  -- putStrLn $ cs $ Aeson.encode m
  WS.runServer address port $ application state
