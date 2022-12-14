module Api.Types.CTodoListItem where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson

data CTodoListItem = CTodoListItem
  { name :: T.Text
  , userId :: Integer
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)