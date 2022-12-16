module Handlers where

import qualified Messages as Msg
import qualified Standard.Messages as Msg
import qualified Standard.Communication as Comm
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

handleWSClientMessage :: T.Text -> Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
handleWSClientMessage msg clientId state = do
  clients <- Conc.readMVar state
  let mClient = Comm.getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  
  -- Useful for debugging
  T.putStrLn $ "[handleWSClientMessage] New msg: " <> msg
  T.putStrLn $ "[handleWSClientMessage] From client: " <> UUID.toText (Comm.wsClientId client)
  T.putStrLn $ "[handleWSClientMessage] From client: " <> cs (show (Comm.userId client))

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

-- TODO: fix the error handling structure. MTL library? Control.Monad.Except? Control.Monad.Cont? Control.Exception?
handleReqRegister :: Msg.ReqRegister -> Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
handleReqRegister reqRegister clientId state = do
  clients <- Conc.readMVar state
  let mClient = Comm.getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  let email = T.toLower (Msg.reqRegisterEmail reqRegister)
  case Email.isValid (cs email) of
    False -> do
      print "[error] Invalid email"
      Comm.sendMessage (Comm.conn client) Msg.ErrorRegisterEmail { type_ = Proxy.Proxy, text = "Invalid email"}
    True -> do
      let password = Msg.reqRegisterPassword reqRegister
      mUser <- Db.findUserByEmail email
      case mUser of 
        Just user -> do
          print "[error] User already registered"
          Comm.sendMessage (Comm.conn client) Msg.ErrorRegisterEmail { type_ = Proxy.Proxy, text = "User already registered"}
        Nothing -> do
          case validatePassword password of
            False -> do
              print "[error] Invalid password"
              Comm.sendMessage (Comm.conn client) Msg.ErrorRegisterPassword { type_ = Proxy.Proxy, text = "Password must be at least 8 characters in length"}
            True -> do
              mUser <- Db.createUser CUser.CUser
                { email = email
                , password = password
                }
              case mUser of
                Nothing -> do
                  Comm.sendMessage (Comm.conn client) Msg.ErrorRegisterEmail { type_ = Proxy.Proxy, text = "Invalid email"}
                  print "[error] Couldn't create user"
                Just user -> do
                  -- (also create and send session, logging in the user)
                  mSession <- Db.createSession user
                  case mSession of
                    Nothing -> do
                      Comm.sendMessage (Comm.conn client) Msg.ErrorRegisterEmail { type_ = Proxy.Proxy, text = "Something went wrong :("}
                      print "[error] Auth failed, couldn't create session"
                    Just session -> do
                      -- update the client in state to have the user ID
                      let newClient = client { Comm.userId = Just (RUser.id user) }
                      Conc.modifyMVar_ state $ \s -> do
                        let s' = Comm.updateWSClient newClient s
                        return s'
                      Comm.sendMessage (Comm.conn client) Msg.ResRegister { type_ = Proxy.Proxy, resRegisterSessionId = RSession.id session  }

validatePassword :: T.Text -> Bool
validatePassword p = T.length p >= 8

handleReqSignIn :: Msg.ReqSignIn -> Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
handleReqSignIn reqSignIn clientId state = do
  clients <- Conc.readMVar state
  let mClient = Comm.getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  let email = T.toLower (Msg.reqSignInEmail reqSignIn)
  let password = Msg.reqSignInPassword reqSignIn
  mUser <- Db.authenticateUser email password
  case mUser of
    Nothing -> do
      print "[error] Auth failed, no user found"
      Comm.sendMessage (Comm.conn client) Msg.ErrorSignIn { type_ = Proxy.Proxy, text = "Incorrect email or password"}
    Just user -> do -- auth succeeded
      mSession <- Db.createSession user
      case mSession of
        Nothing -> do
          print "[error] Auth failed, couldn't create session"
          Comm.sendMessage (Comm.conn client) Msg.ErrorSignIn { type_ = Proxy.Proxy, text = "Something went wrong :("}
        Just session -> do
          -- update the client in state to have the user ID
          let newClient = client { Comm.userId = Just (RUser.id user) }
          Conc.modifyMVar_ state $ \s -> do
            let s' = Comm.updateWSClient newClient s
            return s'
          Comm.sendMessage (Comm.conn client) Msg.ResSignIn { type_ = Proxy.Proxy, resSignInSessionId = RSession.id session }

handleReqSignOut :: Msg.ReqSignOut -> Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
handleReqSignOut reqSignOut clientId state = do
  clients <- Conc.readMVar state
  let mClient = Comm.getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  let sessionId = Msg.reqSignOutSessionId reqSignOut
  Db.deleteSession sessionId
  Comm.sendMessage (Comm.conn client) Msg.ResSignOut { type_ = Proxy.Proxy }

handleReqTodoList :: Msg.ReqTodoList -> Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
handleReqTodoList reqTodoList clientId state = do
  clients <- Conc.readMVar state
  let mClient = Comm.getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  -- get user to be able to query only user's todos
  let mSessionId = Msg.reqTodoListSessionId reqTodoList
  case mSessionId of
    Nothing -> do
      print "[error] Invalid session"
      Comm.sendMessage (Comm.conn client) Msg.ResSignOut { type_ = Proxy.Proxy }
    Just sessionId -> do
      mUserId <- Db.findUserIdBySessionId sessionId
      case mUserId of
        Nothing -> do
          print "[error] Couldn't find user of session"
          Db.deleteSession sessionId
          Comm.sendMessage (Comm.conn client) Msg.ResSignOut { type_ = Proxy.Proxy }
        Just userId -> do
          let newWSClient = client { Comm.userId = Just userId }
          Conc.modifyMVar_ state $ \s -> do
            let s' = Comm.updateWSClient newWSClient s
            return s'
          sendTodoList (Comm.wsClientId newWSClient) state

handleReqCreateTodo :: Msg.ReqCreateTodo -> Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
handleReqCreateTodo reqCreateTodo clientId state = do
  clients <- Conc.readMVar state
  let mClient = Comm.getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  let name = (Msg.name :: Msg.ReqCreateTodo -> T.Text) reqCreateTodo
  let sessionId = (Msg.reqCreateTodoSessionId :: Msg.ReqCreateTodo -> T.Text) reqCreateTodo
  mSession <- Db.findSessionById sessionId
  case mSession of
    Nothing -> do
      print "[error] Auth failed, couldn't retrieve session"
      Comm.sendMessage (Comm.conn client) Msg.ResSignOut { type_ = Proxy.Proxy }
    Just session -> do
      let userId = RSession.userId session
      mTodo <- Db.createTodo CTodoListItem.CTodoListItem
        { name = name
        , userId = userId
        }
      case mTodo of
        Nothing -> do
          print "[error] Couldn't create todo item"
          Comm.sendMessage (Comm.conn client) Msg.ErrorCreateTodo { type_ = Proxy.Proxy, text = "Something went wrong :("}
        Just todo -> do
          Comm.sendMessage (Comm.conn client) Msg.ResCreateTodo { type_ = Proxy.Proxy }

handleReqDeleteTodo :: Msg.ReqDeleteTodo -> Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
handleReqDeleteTodo reqDeleteTodo clientId state = do
  clients <- Conc.readMVar state
  let mClient = Comm.getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  Db.deleteTodo $ Msg.reqDeleteTodoId reqDeleteTodo
  Comm.sendMessage (Comm.conn client) Msg.ResDeleteTodo { type_ = Proxy.Proxy }

handleReqToggleTodo :: Msg.ReqToggleTodo -> Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
handleReqToggleTodo reqToggleTodo clientId state = do
  clients <- Conc.readMVar state
  let mClient = Comm.getWSClient clientId clients
  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[handleWSClientMessage] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient
  let itemId = Msg.reqToggleTodoId reqToggleTodo
  let checked = Msg.checked reqToggleTodo
  Db.updateTodo itemId UTodoListItem.UTodoListItem
    { checked = checked
    }
  Comm.sendMessage (Comm.conn client) Msg.ResToggleTodo { type_ = Proxy.Proxy }

sendTodoList :: Comm.WSClientId -> Conc.MVar Comm.ServerState -> IO ()
sendTodoList clientId state = do
  clients <- Conc.readMVar state
  let mClient = Comm.getWSClient clientId clients

  M.when (Maybe.isNothing mClient) $ do
    T.putStrLn $ "[sendTodoList] Error: client not in state: " <> UUID.toText clientId
  M.guard (Maybe.isJust mClient)
  let client = Maybe.fromJust mClient

  items <- case Comm.userId client of
    Nothing -> return []
    Just userId' -> Db.getTodoList userId'

  let msg = Msg.ResTodoList { type_ = Proxy.Proxy, items = items }
  Comm.sendMessage (Comm.conn client) msg

  let updateMsg = Msg.UpdateTodoList { type_ = Proxy.Proxy, items = items }
  Comm.broadcastUserMessage state client updateMsg