{-# LANGUAGE OverloadedStrings #-}


module Main where

import qualified Data.Map.Strict  as Map
import           Data.Yaml.Syck
import           SlackBot.HiveBot
import           SlackBot.Types
import           Web.Slack

myConfig :: FilePath -> IO SlackConfig
myConfig fn = do
  c  <- parseYamlFile fn
  let EMap[(_,bTokenEl)] = n_elem c
      EStr token = n_elem bTokenEl
  return SlackConfig { _slackApiToken = unpackBuf token }

main :: IO ()
main = do
  c <- myConfig "SlackBotConfig.yaml"
  runBot c hiveBot (SlackBotState Map.empty)
