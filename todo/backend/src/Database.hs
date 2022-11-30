-- The database is stored in a file, created locally in the system: backend/todo.db
module Database where

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite3 as SQLite
import qualified Data.Text.IO as T
import qualified Data.Text as T

import qualified Api.Types.RTodoListItem as RTodoListItem
import qualified Api.Types.CTodoListItem as CTodoListItem
import qualified Api.Types.UTodoListItem as UTodoListItem
import qualified Api.Types.CUser as CUser
import qualified Api.Types.RUser as RUser
import qualified Database.Types.TodoListItem as DbTodoListItem
import qualified Database.Types.User as DbUser

dbFile :: FilePath
dbFile = "todo.db"

schemaFile :: FilePath
schemaFile = "src/Database/Schema.sql"

seedsFile :: FilePath
seedsFile = "src/Database/Seeds.sql"

connect :: IO SQL.Connection
connect = SQL.open dbFile

build :: IO ()
build = do
  conn <- connect
  let db = SQL.connectionHandle conn
  schemaSQL <- T.readFile schemaFile
  SQLite.exec db schemaSQL
  putStrLn $ "Schema assembled"
  SQL.close conn

populate :: IO ()
populate = do
  conn <- connect
  let db = SQL.connectionHandle conn
  seedsSQL <- T.readFile seedsFile
  SQLite.exec db seedsSQL
  putStrLn $ "DB populated"
  SQL.close conn

getTodoList :: IO [RTodoListItem.RTodoListItem]
getTodoList = do
  conn <- connect
  items :: [DbTodoListItem.TodoListItem] <- SQL.query_ conn "SELECT * FROM TodoListItem"
  let apiItems :: [RTodoListItem.RTodoListItem] = flip map items $ \item ->
        RTodoListItem.RTodoListItem
        { RTodoListItem.id = DbTodoListItem.id item
        , RTodoListItem.name = DbTodoListItem.name item
        , RTodoListItem.checked = DbTodoListItem.checked item == 1
        , RTodoListItem.createdAt = DbTodoListItem.createdAt item
        , RTodoListItem.updatedAt = DbTodoListItem.updatedAt item
        }
  SQL.close conn
  return apiItems

createTodo :: CTodoListItem.CTodoListItem -> IO ()
createTodo todoItem = do
  conn <- connect
  let todo = CTodoListItem.name todoItem
  SQL.execute conn "INSERT INTO TodoListItem (name) VALUES (?)" [todo]
  SQL.close conn

-- readTodo :: Integer -> IO RTodoListItem.RTodoListItem

updateTodo :: Integer -> UTodoListItem.UTodoListItem -> IO ()
updateTodo itemId todoItem = do
  conn <- connect
  let checked = UTodoListItem.checked todoItem
  SQL.execute conn "UPDATE TodoListItem SET (checked) = (?) WHERE id = ?" (checked, itemId)
  SQL.close conn

deleteTodo :: Integer -> IO ()
deleteTodo itemId = do
  conn <- connect
  SQL.execute conn "DELETE FROM TodoListItem WHERE id = ?" [itemId]
  SQL.close conn

-- readUser :: Integer -> IO RUser.RUser

findUserByEmail :: T.Text -> IO (Maybe RUser.RUser)
findUserByEmail email = do
  conn <- connect
  rows :: [DbUser.User] <- SQL.query conn "SELECT * FROM user WHERE email = ?" [email]
  SQL.close conn
  case rows of
    [] -> return Nothing
    (x:_) -> return (Just RUser.RUser
        { RUser.id = DbUser.id x
        , RUser.email = DbUser.email x
        , RUser.password = DbUser.password x
        , RUser.createdAt = DbUser.createdAt x
        , RUser.updatedAt = DbUser.updatedAt x
        }) 

createUser :: CUser.CUser -> IO ()
createUser user = do
  conn <- connect
  let email = CUser.email user
  let password = CUser.password user
  SQL.execute conn "INSERT INTO user (email, password) VALUES (?, ?)" (email, password)
  SQL.close conn

-- readUsers :: IO [RUser.RUser]
-- readUsers = do
--   conn <- connect
--   users :: [DbUser.User] <- SQL.query_ conn "SELECT * FROM user"
