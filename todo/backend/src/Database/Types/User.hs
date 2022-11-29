module Database.Types.User where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Database.SQLite.Simple as SQL
import qualified Data.Time.Clock as Time
-- import qualified Data.CaseInsensitive as CI

data User = User
  { id :: Integer
  , email :: T.Text
  , password :: T.Text
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
  }
  deriving (Generics.Generic, Show, SQL.FromRow, SQL.ToRow)

{-
Want to use CI.CI T.Text for email, but SQLite.Simple doesn't have FromRow or ToRow instances. Can borrow from PostgreSQL.Simple:

https://hackage.haskell.org/package/postgresql-simple-0.6.5/docs/src/Database.PostgreSQL.Simple.FromField.html#FromField
https://hackage.haskell.org/package/postgresql-simple-0.6.5/docs/src/Database.PostgreSQL.Simple.ToField.html
-}