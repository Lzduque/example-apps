module Database.Types.TodoListItem where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Database.SQLite.Simple as SQL
import qualified Data.Time.Clock as Time

data TodoListItem = TodoListItem
  { id :: Integer
  , name :: T.Text
  , checked :: Integer
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
  , userId :: Integer -- foreign key
  }
  deriving (Generics.Generic, Show, SQL.FromRow, SQL.ToRow)