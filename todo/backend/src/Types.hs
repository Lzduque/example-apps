module Types where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson

data TodoListItem = TodoListItem
  { name :: T.Text
  , checked :: Bool
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)