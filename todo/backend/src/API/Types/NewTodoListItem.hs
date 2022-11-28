module Api.Types.NewTodoListItem where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson

data NewTodoListItem = NewTodoListItem
  { name :: T.Text
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)