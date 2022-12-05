module Messages where

import qualified Data.Text as T
import qualified GHC.Generics as Generics
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:))
import qualified GHC.TypeLits as TypeLits
import qualified Data.Proxy as Proxy
import Data.String.Conversions (cs)
import qualified Control.Monad as M
import qualified Api.Types.RTodoListItem as RTodoListItem

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

data ReqTodoList = ReqTodoList
  { type_ :: Proxy.Proxy "ReqTodoList"
  , reqTodoListSessionId :: Maybe T.Text
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ResTodoList = ResTodoList
  { type_ :: Proxy.Proxy "ResTodoList"
  , items :: [RTodoListItem.RTodoListItem]
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ReqCreateTodo = ReqCreateTodo
  { type_ :: Proxy.Proxy "ReqCreateTodo"
  , name :: T.Text
  , reqCreateTodoSessionId :: T.Text
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ResCreateTodo = ResCreateTodo
  { type_ :: Proxy.Proxy "ResCreateTodo"
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ReqDeleteTodo = ReqDeleteTodo
  { type_ :: Proxy.Proxy "ReqDeleteTodo"
  , reqDeleteTodoId :: Integer
  , reqDeleteTodoSessionId :: T.Text
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ResDeleteTodo = ResDeleteTodo
  { type_ :: Proxy.Proxy "ResDeleteTodo"
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ReqToggleTodo = ReqToggleTodo
  { type_ :: Proxy.Proxy "ReqToggleTodo"
  , reqToggleTodoId :: Integer
  , checked :: Bool
  , reqToggleTodoSessionId :: T.Text
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ResToggleTodo = ResToggleTodo
  { type_ :: Proxy.Proxy "ResToggleTodo"
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ReqRegister = ReqRegister
  { type_ :: Proxy.Proxy "ReqRegister"
  , reqRegisterEmail :: T.Text
  , reqRegisterPassword :: T.Text
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ResRegister = ResRegister
  { type_ :: Proxy.Proxy "ResRegister"
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ReqSignIn = ReqSignIn
  { type_ :: Proxy.Proxy "ReqSignIn"
  , reqSignInEmail :: T.Text
  , reqSignInPassword :: T.Text
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ResSignIn = ResSignIn
  { type_ :: Proxy.Proxy "ResSignIn"
  , sessionId :: T.Text
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ReqSignOut = ReqSignOut
  { type_ :: Proxy.Proxy "ReqSignOut"
  , reqSignOutSessionId :: T.Text
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)

data ResSignOut = ResSignOut
  { type_ :: Proxy.Proxy "ResSignOut"
  }
  deriving (Generics.Generic, Show, Aeson.ToJSON, Aeson.FromJSON)