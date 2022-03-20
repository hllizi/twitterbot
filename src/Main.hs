{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

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
import RIO (UnliftIO (unliftIO))
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

getTwinfo :: IO TWInfo
getTwinfo = do
  tokens <- getTokens
  credentials <- getCredentials
  return $ setCredential tokens credentials def

getManager =
  Tw.newManager tlsManagerSettings

triggerTweetIds = do
  mgr <- getManager
  twinfo <- getTwinfo
  tweets <- call twinfo mgr (searchTweets "Kitzbühel")
  return $ view statusId <$> view searchResultStatuses tweets

randomIndex :: Int -> IO Int
randomIndex max = do
  let gen = mkStdGen 7
  randomRIO (1, max)

type ScreenName = Text
performOnTracked :: [Text] -> ((StatusId, ScreenName) -> IO ()) -> IO ()
performOnTracked keywords action = do
  twinfo <- getTwinfo
  mgr <- getManager
  phrases <- getPhrases
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
        .| mapMC (liftIO . action)
        .| sinkNull

  return ()

getPhrases :: IO [Text]
getPhrases = do
  input
    auto
    phrasesFile

replyTo :: (StatusId, ScreenName) -> IO ()
replyTo (id, screenName) = do
  twinfo <- getTwinfo
  mgr <- getManager
  phrases <- getPhrases
  index <- randomIndex (length phrases - 1)
  let replyText = "@" <> screenName <> " " <> (phrases !! index)
  call twinfo mgr (update replyText & #in_reply_to_status_id ?~ id)
  return ()

main :: IO ()
main = do
  --timeline <- call twinfo mgr statusesHomeTimeline
  performOnTracked ["Kitzbühel"] replyTo
  return ()
