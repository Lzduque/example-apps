module Standard.Messages where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import qualified GHC.TypeLits as TypeLits
import qualified Data.Proxy as Proxy
import Data.String.Conversions (cs)
import qualified Control.Monad as M

instance {-# OVERLAPPING #-} TypeLits.KnownSymbol s =>Aeson.ToJSON (Proxy.Proxy s) where
  toJSON p = Aeson.String (cs $ TypeLits.symbolVal p)

instance {-# OVERLAPPING #-} TypeLits.KnownSymbol s => Aeson.FromJSON (Proxy.Proxy s) where
  parseJSON (Aeson.String s)
    | s == cs (TypeLits.symbolVal (Proxy.Proxy :: Proxy.Proxy s))
      = return (Proxy.Proxy :: Proxy.Proxy s)
  parseJSON _ = M.mzero

data ReqConnection = ReqConnection
  { type_ :: Proxy.Proxy "ReqConnection"
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ResConnection = ResConnection
  { type_ :: Proxy.Proxy "ResConnection"
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)