module Main where

-- import qualified MyLib as Lib
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

type UserId = T.Text
type Client = (UserId, WS.Connection)
type ServerState = [Client]

data ConnectionMessage = ConnectionMessage {
  type_ :: T.Text
  , userId :: T.Text
} deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

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
  T.putStrLn message
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
  let client = parseConnectionMessage msg
  if
    | Maybe.isJust client -> do
      handleNewConnection (Maybe.fromJust client) conn state
    | otherwise -> do
      T.putStrLn $ "Message not recognized: " <> msg
  where
    parseConnectionMessage :: T.Text -> Maybe Client
    parseConnectionMessage msg = Just ("1234", conn) -- TODO with JSON parsing (aeson)

handleNewConnection
  :: Client
  -> WS.Connection
  -> Conc.MVar ServerState 
  -> IO ()
handleNewConnection client conn state = do
  clients <- Conc.readMVar state
  if
    | clientExists client clients -> do
      T.putStrLn $ "User already exists: " <> fst client
    | otherwise -> flip Except.finally (disconnect client state) $ do
      Conc.modifyMVar_ state $ \s -> do
        let s' = addClient client s
        broadcast (fst client <> " joined") s'
        return s'
      talk client state

disconnect :: Client -> Conc.MVar ServerState -> IO ()
disconnect client state = do
  -- Remove client and return new state
  s <- Conc.modifyMVar state $ \s -> do
    let s' = removeClient client s
    return (s', s')
  broadcast (fst client <> " disconnected") s

talk :: Client -> Conc.MVar ServerState -> IO ()
talk (user, conn) state = M.forever $ do
  msg <- WS.receiveData conn
  Conc.readMVar state >>= broadcast
    (user `mappend` ": " `mappend` msg)


main :: IO ()
main = do
  state <- Conc.newMVar newServerState
  let address :: String = "127.0.0.1"
  let port :: Int = 9160
  putStrLn $
    "Listening on " <> address <> ":" <> show port
  WS.runServer address port $ application state
