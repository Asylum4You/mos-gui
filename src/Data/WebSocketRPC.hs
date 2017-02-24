{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , OverloadedStrings
  , NamedFieldPuns
  #-}

module Data.WebSocketRPC where

import qualified Data.Text as T
import Data.Aeson (FromJSON (..), ToJSON (..), (.:), Value (Object), object, (.=))
import Data.Aeson.Types (typeMismatch, Parser)
import Control.Applicative ((<|>))


-- | Identifier chosen by the client, to uniquely identify RPC sessions
newtype RPCIdent = RPCIdent { getRPCIdent :: Integer }
  deriving (Show, Read, Eq, Ord, Enum, FromJSON, ToJSON)


 -- | Initial call from client to server
data Subscribe a = Subscribe
  { _sub_ident  :: RPCIdent
  , _sub_method :: T.Text
  , _sub_params :: a
  } deriving (Show, Read, Eq)

instance FromJSON a => FromJSON (Subscribe a) where
  parseJSON (Object o) = do
    r <- o .: "rpc"
    if r == ("sub" :: T.Text)
    then Subscribe <$> o .: "ident" <*> o .: "method" <*> o .: "params"
    else fail "not a subscription"
  parseJSON x = typeMismatch "Subscribe" x

instance ToJSON a => ToJSON (Subscribe a) where
  toJSON Subscribe {_sub_ident, _sub_method, _sub_params} =
    object
      [ "ident"  .= _sub_ident
      , "method" .= _sub_method
      , "params" .= _sub_params
      , "rpc"    .= ("sub" :: T.Text)
      ]



newtype Cancel = Cancel {getCancel :: Bool}
  deriving (Show, Read, Eq, FromJSON, ToJSON)

-- | Continued call from client to server
data Supply a = Supply
  { _sup_ident  :: RPCIdent
  , _sup_params :: a
  , _sup_cancel :: Cancel
  } deriving (Show, Read, Eq)

instance FromJSON a => FromJSON (Supply a) where
  parseJSON (Object o) = do
    r <- o .: "rpc"
    if r == ("sup" :: T.Text)
    then Supply <$> o .: "ident" <*> o .: "params" <*> o .: "cancel"
    else fail "not a supply"
  parseJSON x = typeMismatch "Supply" x

instance ToJSON a => ToJSON (Supply a) where
  toJSON Supply {_sup_ident, _sup_params, _sup_cancel} =
    object
      [ "ident"  .= _sup_ident
      , "params" .= _sup_params
      , "cancel" .= _sup_cancel
      , "rpc"    .= ("sup" :: T.Text)
      ]


-- | Continued response from server to client
data Reply a = Reply
  { _rep_ident  :: RPCIdent
  , _rep_params :: a
  } deriving (Show, Read, Eq)

instance FromJSON a => FromJSON (Reply a) where
  parseJSON (Object o) = do
    r <- o .: "rpc"
    if r == ("rep" :: T.Text)
    then Reply <$> o .: "ident" <*> o .: "params"
    else fail "not a reply"
  parseJSON x = typeMismatch "Reply" x

instance ToJSON a => ToJSON (Reply a) where
  toJSON Reply {_rep_ident, _rep_params} =
    object
      [ "ident"  .= _rep_ident
      , "params" .= _rep_params
      , "rpc"    .= ("rep" :: T.Text)
      ]


-- | Final response from server to client
data Complete a = Complete
  { _com_ident  :: RPCIdent
  , _com_params :: a
  } deriving (Show, Read, Eq)

instance FromJSON a => FromJSON (Complete a) where
  parseJSON (Object o) = do
    r <- o .: "rpc"
    if r == ("com" :: T.Text)
    then Complete <$> o .: "ident" <*> o .: "params"
    else fail "not a complete"
  parseJSON x = typeMismatch "Complete" x

instance ToJSON a => ToJSON (Complete a) where
  toJSON Complete {_com_ident, _com_params} =
    object
      [ "ident"  .= _com_ident
      , "params" .= _com_params
      , "rpc"    .= ("com" :: T.Text)
      ]



data IncomingWSRPC
  = WSSub (Subscribe ()) -- FIXME extend with actual method DSL
  | WSSup (Supply ())
  | WSPong
  deriving (Show, Eq)

instance FromJSON IncomingWSRPC where
  parseJSON x = (WSSub <$> parseJSON x)
            <|> (WSSup <$> parseJSON x)
            <|> (WSPong <$ (parseJSON x :: Parser ()))

data OutgoingWSRPC
  = WSRep (Reply ())
  | WSCom (Complete ())
  | WSPing
  deriving (Show, Eq)

instance ToJSON OutgoingWSRPC where
  toJSON (WSRep r) = toJSON r
  toJSON (WSCom r) = toJSON r
  toJSON WSPing = toJSON ()
