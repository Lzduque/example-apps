-- The database is stored in a file, created locally in the system: backend/todo.db
module Database where

import qualified Database.SQLite.Simple as SQL
import qualified Database.SQLite3 as SQLite
import qualified Data.Text.IO as T

import qualified Api.Types.TodoListItem as TodoListItem
import qualified Database.Types.TodoListItem as DbTodoListItem

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

getTodoList :: IO [TodoListItem.TodoListItem]
getTodoList = do
  conn <- connect
  items :: [DbTodoListItem.TodoListItem] <- SQL.query_ conn "SELECT * FROM TodoListItem"
  let apiItems :: [TodoListItem.TodoListItem] = flip map items $
    \item -> TodoListItem.TodoListItem
      { TodoListItem.id = DbTodoListItem.id item
      , TodoListItem.name = DbTodoListItem.name item
      , TodoListItem.checked = DbTodoListItem.checked item == 1
      }
  SQL.close conn
  return apiItems