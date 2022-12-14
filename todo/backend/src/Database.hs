-- The database is stored in a file, created locally in the system: backend/todo.db
module Database where

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite3 as SQLite
import qualified Data.Text.IO as T
import qualified Data.Text as T
import qualified Crypto.BCrypt as Crypto
import qualified Data.Maybe as Maybe
import Data.String.Conversions (cs)
import qualified Control.Exception as E
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID

import qualified Api.Types.RTodoListItem as RTodoListItem
import qualified Api.Types.CTodoListItem as CTodoListItem
import qualified Api.Types.UTodoListItem as UTodoListItem
import qualified Api.Types.CUser as CUser
import qualified Api.Types.RUser as RUser
import qualified Api.Types.RSession as RSession
import qualified Database.Types.TodoListItem as DbTodoListItem
import qualified Database.Types.User as DbUser
import qualified Database.Types.Session as DbSession
import qualified Standard.Database as Db

getTodoList :: Integer -> IO [RTodoListItem.RTodoListItem]
getTodoList userId = do
  conn <- Db.connect
  items :: [DbTodoListItem.TodoListItem] <- Db.handleQuery 
    (SQL.query conn "SELECT * FROM TodoListItem WHERE userId = ?" [userId :: Integer]
    )
    []
  let apiItems :: [RTodoListItem.RTodoListItem] = flip map items $ \item ->
        RTodoListItem.RTodoListItem
        { RTodoListItem.id = DbTodoListItem.id item
        , RTodoListItem.name = DbTodoListItem.name item
        , RTodoListItem.checked = DbTodoListItem.checked item == 1
        , RTodoListItem.createdAt = DbTodoListItem.createdAt item
        , RTodoListItem.updatedAt = DbTodoListItem.updatedAt item
        , RTodoListItem.userId = DbTodoListItem.userId item
        }
  SQL.close conn
  return apiItems

createTodo :: CTodoListItem.CTodoListItem -> IO (Maybe RTodoListItem.RTodoListItem)
createTodo todoItem = do
  conn <- Db.connect
  let name = T.strip (CTodoListItem.name todoItem)
  let userId = CTodoListItem.userId todoItem
  rows :: [DbTodoListItem.TodoListItem] <- Db.handleQuery 
    (SQL.query conn "INSERT INTO TodoListItem (name, userId) VALUES (?, ?) RETURNING *" (name :: T.Text, userId :: Integer))
    []
  SQL.close conn
  case rows of
    [] -> return Nothing
    (x:_) -> return (Just RTodoListItem.RTodoListItem
      { RTodoListItem.id = DbTodoListItem.id x
      , RTodoListItem.name = DbTodoListItem.name x
      , RTodoListItem.checked = DbTodoListItem.checked x == 1
      , RTodoListItem.createdAt = DbTodoListItem.createdAt x
      , RTodoListItem.updatedAt = DbTodoListItem.updatedAt x
      , RTodoListItem.userId = DbTodoListItem.userId x
      })

-- readTodo :: Integer -> IO RTodoListItem.RTodoListItem

updateTodo :: Integer -> UTodoListItem.UTodoListItem -> IO ()
updateTodo itemId todoItem = do
  conn <- Db.connect
  let checked = UTodoListItem.checked todoItem
  Db.handleQuery 
    (SQL.execute conn "UPDATE TodoListItem SET (checked) = (?) WHERE id = ?"
      (checked :: Bool
      , itemId :: Integer
      )
    )
    ()
  SQL.close conn

deleteTodo :: Integer -> IO ()
deleteTodo itemId = do
  conn <- Db.connect
  Db.handleQuery 
    (SQL.execute conn "DELETE FROM TodoListItem WHERE id = ?" [itemId :: Integer])
    ()
  SQL.close conn

-- readUser :: Integer -> IO RUser.RUser

findUserByEmail :: T.Text -> IO (Maybe RUser.RUser)
findUserByEmail email = do
  conn <- Db.connect
  rows :: [DbUser.User] <- Db.handleQuery (SQL.query conn "SELECT * FROM User WHERE email = ?" [email :: T.Text]) []
  SQL.close conn
  case rows of
    [] -> return Nothing
    (x:_) -> return (Just RUser.RUser
      { RUser.id = DbUser.id x
      , RUser.email = DbUser.email x
      , RUser.createdAt = DbUser.createdAt x
      , RUser.updatedAt = DbUser.updatedAt x
      }) 

createUser :: CUser.CUser -> IO (Maybe RUser.RUser)
createUser user = do
  conn <- Db.connect
  let email = T.strip (CUser.email user)
  let password = CUser.password user
  hashedPassword <- Crypto.hashPasswordUsingPolicy Crypto.fastBcryptHashingPolicy (cs password)
  rows :: [DbUser.User] <- Db.handleQuery 
    (SQL.query conn "INSERT INTO User (email, password) VALUES (?, ?) RETURNING *"
      ( email :: T.Text
      , cs (Maybe.fromJust hashedPassword) :: T.Text)
    )
    []
  SQL.close conn
  case rows of
    [] -> return Nothing
    (x:_) -> return (Just RUser.RUser
      { id = DbUser.id x
      , email = DbUser.email x
      , createdAt = DbUser.createdAt x
      , updatedAt = DbUser.updatedAt x
      })

authenticateUser :: T.Text -> T.Text -> IO (Maybe RUser.RUser)
authenticateUser email password = do
  conn <- Db.connect
  rows :: [DbUser.User] <- Db.handleQuery (SQL.query conn "SELECT * FROM User WHERE email = ?" [email :: T.Text]) []
  SQL.close conn
  case rows of
    [] -> do
      print "Error: no user found" -- TEMP
      return Nothing
    (x:_) -> do
      let dbPassword = DbUser.password x
      case Crypto.validatePassword (cs dbPassword) (cs password) of 
        False -> do
          print "Error: invalid password" -- TEMP
          return Nothing
        True -> return (Just RUser.RUser
          { RUser.id = DbUser.id x
          , RUser.email = DbUser.email x
          , RUser.createdAt = DbUser.createdAt x
          , RUser.updatedAt = DbUser.updatedAt x
          }) 

createSession :: RUser.RUser -> IO (Maybe RSession.RSession)
createSession user = do
  conn <- Db.connect
  let userId :: Integer = RUser.id user
  sId <- UUID.nextRandom
  let sessionId :: T.Text = UUID.toText sId
  rows :: [DbSession.Session] <- Db.handleQuery 
    (SQL.query conn "INSERT INTO Session (id, userId) VALUES (?, ?) RETURNING *"
      ( sessionId :: T.Text
      , userId :: Integer
      )
    )
    []
  SQL.close conn
  case rows of
    [] -> return Nothing
    (x:_) -> return (Just RSession.RSession
      { RSession.id = DbSession.id x
      , RSession.userId = DbSession.userId x
      })

findSessionById :: T.Text -> IO (Maybe RSession.RSession)
findSessionById sessionId = do
  conn <- Db.connect
  rows :: [DbSession.Session] <- Db.handleQuery (SQL.query conn "SELECT * FROM Session WHERE id = ?" [sessionId :: T.Text]) []
  SQL.close conn
  case rows of
    [] -> return Nothing
    (x:_) -> return (Just RSession.RSession
      { RSession.id = DbSession.id x
      , RSession.userId = DbSession.userId x
      }) 

findUserIdBySessionId :: T.Text -> IO (Maybe Integer)
findUserIdBySessionId sessionId = do
  conn <- Db.connect
  rows :: [SQL.Only Integer] <- Db.handleQuery (SQL.query conn "SELECT DISTINCT userId FROM Session WHERE id = ?" [sessionId :: T.Text]) []
  SQL.close conn
  case rows of
    [] -> return Nothing
    (x:_) -> return (Just (SQL.fromOnly x)) 

deleteSession :: T.Text -> IO ()
deleteSession sessionId = do
  conn <- Db.connect
  Db.handleQuery 
    (SQL.execute conn "DELETE FROM Session WHERE id = ?" [sessionId :: T.Text])
    ()
  SQL.close conn

