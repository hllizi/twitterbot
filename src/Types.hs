{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Types where

import Data.ByteString
import Control.Lens
import Control.Lens.TH
import Dhall hiding (auto)
import Dhall.Marshal.Decode
import Web.Twitter.Conduit (OAuth)
import Data.Text as T
import GHC.Generics

customAuto :: (Generic a, GenericFromDhall a (Rep a)) => Decoder a
customAuto = genericAutoWith 
        (defaultInterpretOptions 
            {
                fieldModifier = T.dropWhile (== '_')
                }
                )

data Consumer = Consumer
  { _consKey :: Text,
    _consSecret :: Text 
  }
  deriving (Generic, Show)

instance FromDhall Consumer where
    autoWith _ = customAuto
    

data Access = Access
  { _accToken :: Text,
    _accSecret :: Text
  }
  deriving (Generic, Show)

instance FromDhall Access where
    autoWith _ = customAuto

data OAuthConf = OAuthConf
  { _consumer :: Consumer,
    _access :: Access
  }
  deriving (Generic, Show)

instance FromDhall OAuthConf where
    autoWith _ = customAuto

$(makeLenses ''Consumer)
$(makeLenses ''Access)
$(makeLenses ''OAuthConf)
