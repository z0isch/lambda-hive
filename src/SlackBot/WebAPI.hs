{-# LANGUAGE OverloadedStrings #-}

module SlackBot.WebAPI where

import           Control.Lens
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Network.Wreq         as W
import           Web.Slack

postFileToSlack :: SlackConfig -> Text -> FilePath -> IO (W.Response ByteString)
postFileToSlack config channels file =
  W.post "https://slack.com/api/files.upload"
      [ W.partText "token" $ T.pack $ _slackApiToken $ config
      , W.partText "channels" channels
      , W.partFile "file" file
      ]
