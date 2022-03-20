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
import RIO (RIO, UnliftIO (unliftIO), ask)
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
  { tokens :: OAuth,
    credentials :: Credential
  }

class HasAccess env where
  tokensL :: Lens' env OAuth
  credentialsL :: Lens' env Credential

instance HasAccess Env where
  tokensL = lens tokens (\x y -> x {tokens = y})
  credentialsL = lens credentials (\x y -> x {credentials = y})

getTokens :: IO OAuth
getTokens = do
  accessConfig <- getAccessConfig
  return $
    twitterOAuth
      { oauthConsumerKey = encodeUtf8 $ accessConfig ^. consumer . consKey,
        oauthConsumerSecret = encodeUtf8 $ accessConfig ^. consumer . consSecret
      }

getCredentials :: IO Credential
getCredentials = do
  accessConfig <- getAccessConfig
  return $
    Credential
      [ ("oauth_token", encodeUtf8 $ accessConfig ^. access . accToken),
        ("oauth_token_secret", encodeUtf8 $ accessConfig ^. access . accSecret)
      ]

getAccessConfig :: IO OAuthConf
getAccessConfig = do
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
        .| mapC (\status -> (status ^. statusId, status ^. statusUser . userScreenName))
        .| iterMC (liftIO . print)
        .| mapMC (lift . action)
        .| sinkNull

  return ()

getPhrases :: IO [Text]
getPhrases = do
  input
    auto
    phrasesFile

replyTo :: HasAccess env => (StatusId, ScreenName) -> RIO env ()
replyTo (id, screenName) = do
  twinfo <- getTwinfo
  mgr <- liftIO getManager
  phrases <- liftIO getPhrases
  liftIO $ do
    index <- randomIndex (length phrases - 1)
    let replyText = "@" <> screenName <> " " <> (phrases !! index)
    call twinfo mgr (update replyText & #in_reply_to_status_id ?~ id)
    return ()

main :: IO ()
main = do
  --timeline <- call twinfo mgr statusesHomeTimeline
  runRIO $ performOnTracked ["Kitzbühel"] replyTo
