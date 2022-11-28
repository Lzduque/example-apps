module Api.Types.RTodoListItem where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time

data RTodoListItem = RTodoListItem
  { id :: Integer
  , name :: T.Text
  , checked :: Bool
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

-- RTodoListItem
-- { id :: Integer []
-- , name :: Text [Create]
-- , checked :: Bool [Update + default False]
-- }