module Database.Types.Session where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Database.SQLite.Simple as SQL
import qualified Data.Time.Clock as Time

data Session = Session
  { id :: T.Text
  , userId :: Integer -- foreign key
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
  }
  deriving (Generics.Generic, Show, SQL.FromRow, SQL.ToRow)
