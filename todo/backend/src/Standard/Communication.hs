module Standard.Communication where

import qualified Messages as Msg
import qualified Standard.Messages as Msg
import qualified Database as Db

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
import qualified Text.Email.Validate as Email

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