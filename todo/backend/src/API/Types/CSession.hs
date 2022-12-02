module Api.Types.CSession where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson

data CSession = CSession
  { id :: T.Text
  , userId :: T.Text
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)