{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

import Conduit
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy
import Dhall
import Network.HTTP.Conduit as HTTP
import RIO (RIO, UnliftIO (unliftIO), ask, runRIO)
import RIO.Text (decodeUtf8', encodeUtf8)
import System.Directory
import System.FilePath
import System.Process
import System.Random.Stateful
import Types
import Web.Twitter.Conduit as Tw
import Web.Twitter.Conduit.Parameters
import Web.Twitter.Conduit.Status
import Web.Twitter.Types.Lens

configFile = "/home/dlahm/Projekte/TwitterBot/config/twitterbot.dhall"

phrasesFile = "/home/dlahm/Projekte/TwitterBot/config/phrases.dhall"

($^.) :: Functor m => m a -> Getting b a b -> m b
($^.) m f = fmap (^. f) m

data Env = Env
  {  tokens :: OAuth
   , credentials :: Credential
   , botConfig :: BotConfig
  }

class HasAccess env where
  tokensL :: Lens' env OAuth
  credentialsL :: Lens' env Credential

class HasBotConfig env where
  botConfigL :: Lens' env BotConfig

instance HasAccess Env where
  tokensL = lens tokens (\x y -> x {tokens = y})
  credentialsL = lens credentials (\x y -> x {credentials = y})

instance HasBotConfig Env where
    botConfigL = lens botConfig (\x y -> x {botConfig = y})

getTokens :: IO OAuth
getTokens = do
  config <- getConfig
  return $
    twitterOAuth
      { oauthConsumerKey = encodeUtf8 $ config ^. auth . consumer . consKey,
        oauthConsumerSecret = encodeUtf8 $ config ^. auth . consumer . consSecret
      }

getCredentials :: IO Credential
getCredentials = do
  config <- getConfig
  return $
    Credential
      [ ("oauth_token", encodeUtf8 $ config ^. auth. access . accToken),
        ("oauth_token_secret", encodeUtf8 $ config ^. auth . access . accSecret)
      ]

getBotConfig :: IO BotConfig
getBotConfig = do 
  config <- getConfig
  return $ config ^. dhallBotConfig

getConfig :: IO Config
getConfig = do
  input
    auto
    configFile

getTwinfo :: HasAccess m => RIO m TWInfo
getTwinfo = do
  tokens <- ask $^. tokensL
  credentials <- ask $^. credentialsL
  return $ setCredential tokens credentials def

getManager =
  Tw.newManager tlsManagerSettings

randomIndex :: Int -> IO Int
randomIndex max = do
  let gen = mkStdGen 7
  randomRIO (1, max)

type ScreenName = Text

performOnTracked :: HasAccess m => [Text] -> ((StatusId, ScreenName) -> RIO m ()) -> RIO m ()
performOnTracked keywords action = do
  twinfo <- getTwinfo
  mgr <- liftIO getManager
  phrases <- liftIO getPhrases
  runResourceT $ do
    tweetStream <- stream twinfo mgr (statusesFilter [Track keywords])
    runConduit $
      tweetStream
        .| concatMapC
          ( \case
              SStatus status -> Just status
              _ -> Nothing
          )
        .| filterC ((/= 1497897132048760839) . view (statusUser . userId))
        .| mapC (
                    \status -> (
                        status ^. statusId, 
                        status ^. statusUser . userScreenName
                        )
                )
        .| iterMC (liftIO . print)
        .| mapMC (lift . action)
        .| sinkNull

  return ()

getPhrases :: IO [Text]
getPhrases = do
  input
    auto
    phrasesFile

replyTo :: HasAccess env => [Text] -> (StatusId, ScreenName) -> RIO env ()
replyTo phrases (id, screenName) = do
  twinfo <- getTwinfo
  mgr <- liftIO getManager
  liftIO $ do
    index <- randomIndex (length phrases - 1)
    let replyText = "@" <> screenName <> " " <> (phrases !! index)
    call twinfo mgr (update replyText & #in_reply_to_status_id ?~ id)
    return ()

main :: IO ()
main = do
  env <- Env <$> getTokens 
             <*> getCredentials
             <*> getBotConfig
  --timeline <- call twinfo mgr statusesHomeTimeline
  runRIO env $ do 
    triggeredPhrasesConfigs <- ask $^. (botConfigL . triggeredPhrases)
    mapM_ (\triggeredPhrasesConfig -> do
                performOnTracked  
                    (triggeredPhrasesConfig ^. triggers) 
                    (replyTo (triggeredPhrasesConfig ^. phrases)))
          triggeredPhrasesConfigs
