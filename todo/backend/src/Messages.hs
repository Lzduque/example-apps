module Messages where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson

data InConnectionType = InConnectionType
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data InConnection = InConnection {
  type_ :: InConnectionType
  , userId :: T.Text
} deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data InTodoListType = InTodoListType
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data InTodoList = InTodoList {
  type_ :: InTodoListType
} deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

