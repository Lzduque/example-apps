module Api.Types.RUser where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson
import qualified Data.Time.Clock as Time

data RUser = RUser
  { id :: Integer
  , email :: T.Text
  , password :: T.Text
  , createdAt :: Time.UTCTime
  , updatedAt :: Time.UTCTime
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)