module Main where

-- import qualified MyLib as Lib
import qualified Network.WebSockets as WS
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Control.Monad as M
import qualified Control.Concurrent as Conc
import qualified Data.Char as Char
import qualified Control.Exception as Except

type Client = (T.Text, WS.Connection)
type ServerState = [Client]

newServerState :: ServerState
newServerState = []

numClients :: ServerState -> Int
numClients = length

clientExists :: Client -> ServerState -> Bool
-- clientExists client = any ((== fst client) . fst)
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
    msg <- WS.receiveData conn
    clients <- Conc.readMVar state
    let prefix = "Hi! I am "
    let client = (T.drop (T.length prefix) msg, conn)
    let disconnect = do
          -- Remove client and return new state
          s <- Conc.modifyMVar state $ \s -> do
            let s' = removeClient client s
            return (s', s')
          broadcast (fst client <> " disconnected") s
    -- the pattern here is to catch errors before and the last condition is, if everything's alright, allow connection
    if
      | not (prefix `T.isPrefixOf` msg) -> do
        WS.sendTextData conn ("Wrong announcement" :: T.Text)
      | any ($ fst client) [T.null, T.any Char.isPunctuation, T.any Char.isSpace] -> do
        WS.sendTextData conn ("Name cannot contain punctuation or whitespace, and cannot be empty" :: T.Text)
      | clientExists client clients -> do
        WS.sendTextData conn ("User already exists" :: T.Text)
      | otherwise -> flip Except.finally disconnect $ do
        Conc.modifyMVar_ state $ \s -> do
          let s' = addClient client s
          WS.sendTextData conn $
            "Welcome! Users: " <>
            T.intercalate ", " (map fst s)
          broadcast (fst client <> " joined") s'
          return s'
        talk client state
    

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
