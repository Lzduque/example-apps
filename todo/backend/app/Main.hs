module Main where

import qualified Standard.Messages as Msg
import qualified Standard.Communication as Comm
import qualified Messages as Msg
import qualified Handlers
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
import qualified Text.Email.Validate as Email

main :: IO ()
main = do
  state <- Conc.newMVar Comm.newServerState
  let address :: String = "127.0.0.1"
  let port :: Int = 9160
  putStrLn $
    "Listening on " <> address <> ":" <> show port
  WS.runServer address port $ application state

application :: Conc.MVar Comm.ServerState -> WS.ServerApp
application state pending = do
  putStrLn "application"
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 30 (return ()) $ do
    msg :: T.Text <- WS.receiveData conn
    T.putStrLn $ "Message received: " <> msg
    handleConnectionMessage msg conn state
    
handleConnectionMessage
  :: T.Text -- ^ Message
  -> WS.Connection 
  -> Conc.MVar Comm.ServerState 
  -> IO ()
handleConnectionMessage msg conn state = do
  let reqConnectionMessage :: Maybe Msg.ReqConnection = Aeson.decode . cs $ msg
  if
    -- Invitation to listen to messages from a specific client
    | Maybe.isJust reqConnectionMessage -> do
      wsClientId <- UUID.nextRandom
      let client = Comm.WSClient 
            { wsClientId = wsClientId
            , userId = Nothing
            , conn = conn
            }
      handleNewConnection client state
    | otherwise -> do
      T.putStrLn $ "Message not recognized (connection): " <> msg

handleNewConnection
  :: Comm.WSClient
  -> Conc.MVar Comm.ServerState 
  -> IO ()
handleNewConnection client state = do
  clients <- Conc.readMVar state
  if
    | Comm.clientExists client clients -> do
      T.putStrLn $ "User already exists: " <> UUID.toText (Comm.wsClientId client)
    | otherwise -> do
      flip Except.finally (disconnect (Comm.wsClientId client) state) $ do
        T.putStrLn $ "[handleNewConnection] From client: " <> UUID.toText (Comm.wsClientId client)
        T.putStrLn $ "[handleNewConnection] From client: " <> cs (show (Comm.userId client))
        Conc.modifyMVar_ state $ \s -> do
          let s' = Comm.addWSClient client s
          return s'
        Comm.sendMessage (Comm.conn client) Msg.ResConnection { type_ = Proxy.Proxy }
        connect (Comm.wsClientId client) state

disconnect :: Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
disconnect clientId state = do
  -- Remove client and return new state
  s <- Conc.modifyMVar state $ \s -> do
    let s' = Comm.removeWSClient clientId s
    return (s', s')
  print "SOMEONE DISCONNECTED"

-- This is to continuously listen for messages from a specific client
connect :: Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
connect clientId state = M.forever $ do
  T.putStrLn $ "[connect] From client: " <> UUID.toText clientId
  clients <- Conc.readMVar state
  T.putStrLn $ "[connect] clients: " <> cs (show clients)
  -- Get the client from the state (match by wsClientId) so we always have its latest version, with any modifications applied to state
  let mWSClient :: Maybe Comm.WSClient = Comm.getWSClient clientId clients
  case mWSClient of
    Nothing -> return ()
    Just client' -> do
      T.putStrLn $ "[connect] From client: " <> UUID.toText (Comm.wsClientId client')
      T.putStrLn $ "[connect] From client: " <> cs (show (Comm.userId client'))
      msg <- WS.receiveData (Comm.conn client')
      Handlers.handleWSClientMessage msg (Comm.wsClientId client') state

