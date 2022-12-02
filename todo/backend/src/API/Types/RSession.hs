module Api.Types.RSession where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson

data RSession = RSession
  { id :: T.Text
  , userId :: Integer
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)