{-# LANGUAGE OverloadedStrings #-}


module Main where

import qualified Data.Map.Strict  as Map
import           SlackBot.HiveBot
import           SlackBot.Types
import           Web.Slack

myConfig :: SlackConfig
myConfig = SlackConfig
        { _slackApiToken = "xoxb-55895900391-OOH0jAgtjH3ehqWwXXXc6Fwk"
        }

main :: IO ()
main = runBot myConfig hiveBot (SlackBotState Map.empty)
