{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import RIO (Exception, HasLogFunc, LogFunc, RIO, SomeException, UnliftIO (unliftIO), ask, asks, catch, displayShow, logFuncL, logInfo, logOptionsHandle, logWarn, runRIO, setLogUseLoc, setLogUseTime, stderr, trace, withLogFunc)
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

($^.) :: Functor m => m a -> Getting b a b -> m b
($^.) m f = fmap (^. f) m

data Env = Env
  { tokens :: !OAuth,
    credentials :: !Credential,
    botConfig :: !BotConfig,
    logFunction :: !LogFunc
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

instance HasLogFunc Env where
  logFuncL = lens logFunction (\x y -> x {logFunction = y})

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

performOnTracked :: (HasLogFunc env, HasAccess env) => [Text] -> ((StatusId, ScreenName) -> RIO env ()) -> RIO env ()
performOnTracked keywords action = do
  twinfo <- getTwinfo
  mgr <- liftIO getManager
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
            .| filterC (  (/= 1497897132048760839) 
                        . view (statusUser . userId))
            .| iterMC (\status -> logInfo $ 
                "Incoming tweet " <> displayShow  (view statusId status))
            .| mapC
              ( \status ->
                  ( status ^. statusId,
                    status ^. statusUser . userScreenName
                  )
              )
            .| mapMC (lift . action)
            .| sinkNull
    )
    ( \(e :: SomeException) -> do
        logWarn
          ( "Could not send Tweet: "
              <> displayShow e
          )
        return ()
    )

makeReply screenName text = "@" <> screenName <> " " <> text

replyTo :: HasAccess env => [Text] -> (StatusId, ScreenName) -> RIO env ()
replyTo phrases (id, screenName) = do
  twinfo <- getTwinfo
  mgr <- liftIO getManager
  liftIO $ do
    index <- randomIndex (length phrases - 1)
    let replyText = makeReply screenName (phrases !! index)
    call twinfo mgr (update replyText & #in_reply_to_status_id ?~ id)
    return ()

test :: HasLogFunc env => RIO env ()
test = do
  logWarn "Pommehero"

main :: IO ()
main = do
  configFile <- execParser optsParser
  logOptions' <- logOptionsHandle stderr False
  let logOptions = setLogUseTime True $ setLogUseLoc True logOptions'
  withLogFunc
      logOptions
      ( \logFunc -> do
          configEnv <-
           runRIO configFile $ do
           Env
                <$> getTokens
                <*> getCredentials
                <*> getBotConfig
          let env = configEnv logFunc
          runRIO env $ do
            twinfo <- getTwinfo
            mgr <- liftIO getManager
            timeline <- liftIO $ call twinfo mgr statusesHomeTimeline
            triggeredPhrasesConfigs <- ask $^. (botConfigL . triggeredPhrases)
            mapM_
                ( \triggeredPhrasesConfig -> do
                    performOnTracked
                        (triggeredPhrasesConfig ^. triggers)
                        (replyTo (triggeredPhrasesConfig ^. phrases))
                )
                triggeredPhrasesConfigs
      )

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
