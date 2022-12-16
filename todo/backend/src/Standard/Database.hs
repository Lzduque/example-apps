-- The database is stored in a file, created locally in the system: backend/todo.db
module Standard.Database where

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

dbFile :: FilePath
dbFile = "src/Database/app.db"

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
  handleQuery (SQLite.exec db schemaSQL) ()
  putStrLn $ "Schema assembled"
  SQL.close conn

populate :: IO ()
populate = do
  conn <- connect
  let db = SQL.connectionHandle conn
  seedsSQL <- T.readFile seedsFile
  handleQuery (SQLite.exec db seedsSQL) ()
  putStrLn $ "DB populated"
  SQL.close conn

handleQuery :: IO a -> a -> IO a
handleQuery query baseValue = E.catch
  query
  (\e -> do
    print $ "SQL Error: " ++ show (e :: E.SomeException)
    return baseValue)