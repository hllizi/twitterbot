{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
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
import Options.Applicative hiding (auto)
import RIO (Exception, SomeException, RIO, UnliftIO (unliftIO), ask, asks, catch, runRIO, trace)
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

configFileDefault = "/etc/twitterbot.dhall"

phrasesFile = "/home/dlahm/Projekte/TwitterBot/config/phrases.dhall"

($^.) :: Functor m => m a -> Getting b a b -> m b
($^.) m f = fmap (^. f) m

data Env = Env
  { tokens :: OAuth,
    credentials :: Credential,
    botConfig :: BotConfig
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

class HasConfigFile env where
  configFileL :: Lens' env String

instance HasConfigFile ConfigFile where
  configFileL = lens conf (\x y -> x {conf = y})

getTokens :: HasConfigFile env => RIO env OAuth
getTokens = do
  config <- getConfig
  return $
    twitterOAuth
      { oauthConsumerKey = encodeUtf8 $ config ^. auth . consumer . consKey,
        oauthConsumerSecret = encodeUtf8 $ config ^. auth . consumer . consSecret
      }

getCredentials :: HasConfigFile env => RIO env Credential
getCredentials = do
  config <- getConfig
  return $
    Credential
      [ ("oauth_token", encodeUtf8 $ config ^. auth . access . accToken),
        ("oauth_token_secret", encodeUtf8 $ config ^. auth . access . accSecret)
      ]

getBotConfig :: HasConfigFile env => RIO env BotConfig
getBotConfig = do
  config <- getConfig
  return $ config ^. dhallBotConfig

getConfig :: HasConfigFile env => RIO env Config
getConfig = do
  configFile <- T.pack <$> asks (view configFileL)
  liftIO $
    input
      auto
      configFile

getTwinfo :: HasAccess env => RIO env TWInfo
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

performOnTracked :: HasAccess env => [Text] -> ((StatusId, ScreenName) -> RIO env ()) -> RIO env ()
performOnTracked keywords action = do
  twinfo <- getTwinfo
  mgr <- liftIO getManager
  phrases <- liftIO getPhrases
  catch
    ( runResourceT $ do
        tweetStream <- stream twinfo mgr (statusesFilter [Track keywords])
        runConduit $
          tweetStream
            .| concatMapC
              ( \case
                  SStatus status -> Just status
                  _ -> Nothing
              )
            .| filterC ((/= 1497897132048760839) . view (statusUser . userId))
            .| iterMC (liftIO . print)
            .| mapC
              ( \status ->
                  ( status ^. statusId,
                    status ^. statusUser . userScreenName
                  )
              )
            .| iterMC (liftIO . print)
            .| mapMC (lift . action)
            .| sinkNull
    )
    (const $ return () :: SomeException -> RIO env ())

getPhrases :: IO [Text]
getPhrases = do
  input
    auto
    phrasesFile

makeReply scrrenName text = "@" <> screenName <> " " <> text

replyTo :: HasAccess env => [Text] -> (StatusId, ScreenName) -> RIO env ()
replyTo phrases (id, screenName) = do
  twinfo <- getTwinfo
  mgr <- liftIO getManager
  liftIO $ do
    index <-  randomIndex (length phrases - 1)
    let replyText = makeReply screenName (phrases !! index)
    call twinfo mgr (update replyText & #in_reply_to_status_id ?~ id)
    return ()

main :: IO ()
main = do
  configFile <- execParser optsParser
  env <-
    runRIO configFile $ do 
     Env 
      <$> getTokens
      <*> getCredentials
      <*> getBotConfig
  --timeline <- call twinfo mgr statusesHomeTimeline
  runRIO env $ do
    triggeredPhrasesConfigs <- ask $^. (botConfigL . triggeredPhrases)
    mapM_
      ( \triggeredPhrasesConfig -> do
          performOnTracked
            (triggeredPhrasesConfig ^. triggers)
            (replyTo (triggeredPhrasesConfig ^. phrases))
      )
      triggeredPhrasesConfigs

--Options

newtype ConfigFile = ConfigFile {conf :: FilePath}

configFileParser :: Parser ConfigFile
configFileParser =
  ConfigFile
    <$> strOption
      ( long "config"
          <> short 'c'
          <> metavar "CONFIGFILE"
          <> help "Config file"
      )

optsParser :: ParserInfo ConfigFile
optsParser =
  info
    configFileParser
    ( fullDesc
        <> progDesc "Twitterbot"
        <> header "good twitterbot"
    )
