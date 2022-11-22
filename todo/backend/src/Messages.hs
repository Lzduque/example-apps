module Messages where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import qualified GHC.TypeLits as TypeLits
import qualified Data.Proxy as Proxy
import Data.String.Conversions (cs)
import qualified Control.Monad as M

data InConnectionType = InConnectionType
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

-- newtype MessageType = MessageType T.Text
--   deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)
newtype MessageType a = MessageType a
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

instance {-# OVERLAPPING #-} TypeLits.KnownSymbol s => Aeson.ToJSON (Proxy.Proxy s) where
    toJSON p = Aeson.String (cs $ TypeLits.symbolVal p)

instance {-# OVERLAPPING #-} TypeLits.KnownSymbol s => Aeson.FromJSON (Proxy.Proxy s) where
  parseJSON (Aeson.String s)
    | s == cs (TypeLits.symbolVal (Proxy.Proxy :: Proxy.Proxy s))
      = return (Proxy.Proxy :: Proxy.Proxy s)
  parseJSON _ = M.mzero

data InConnection = InConnection {
  type_ :: Proxy.Proxy "in-connection"
  , userId :: T.Text
} deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

-- instance Aeson.ToJSON (InConnection) where
--   toJSON o = Aeson.object [ "type_" .= TypeLits.symbolVal ((type_ :: InConnection -> Proxy.Proxy "in-connection") o), "userId" .= (userId o)]

-- data InConnection = InConnection {
--   inConnectionType :: ()
--   , userId :: T.Text
-- } deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data InTodoListType = InTodoListType
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data InTodoList = InTodoList {
  type_ :: InTodoListType
} deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

