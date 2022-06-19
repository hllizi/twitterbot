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

data Secrets = Secrets {
     consumerKey :: !Text
   , consumerSecret :: !Text
   , accessToken :: !Text
   , accessSecret :: !Text
}
    deriving (Generic, Show)

instance ToDhall Secrets

data Consumer = Consumer
  { _consKey :: !Text,
    _consSecret :: !Text 
  }
  deriving (Generic, Show)

instance FromDhall Consumer where
    autoWith _ = customAuto
    

data Access = Access
  { _accToken :: !Text,
    _accSecret :: !Text
  }
  deriving (Generic, Show)

instance FromDhall Access where
    autoWith _ = customAuto

data OAuthConf = OAuthConf
  { _consumer :: !Consumer,
    _access :: !Access
  }
  deriving (Generic, Show)

instance FromDhall OAuthConf where
    autoWith _ = customAuto

data Config = Config 
  {
      _auth :: !OAuthConf
   ,  _dhallBotConfig :: !BotConfig
  }  
  deriving (Generic, Show)

instance FromDhall Config where
    autoWith _ = customAuto

data TriggeredPhrasesConfig = TriggeredPhrasesConfig {
        _phrases :: [Text]
    ,   _triggers :: [Text]
    }
    deriving (Generic, Show)

instance FromDhall TriggeredPhrasesConfig where
    autoWith _ = customAuto

newtype BotConfig = BotConfig {
    _triggeredPhrases :: [TriggeredPhrasesConfig]

}
  deriving (Generic, Show)

instance FromDhall BotConfig where
    autoWith _ = customAuto

$(makeLenses ''Config)
$(makeLenses ''BotConfig)
$(makeLenses ''TriggeredPhrasesConfig)
$(makeLenses ''Consumer)
$(makeLenses ''Access)
$(makeLenses ''OAuthConf)
