module Api.Types.TodoListItem where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson

data TodoListItem = TodoListItem
  { id :: Integer
  , name :: T.Text
  , checked :: Bool
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)