{-# LANGUAGE OverloadedStrings #-}

module SlackBot.Parser where

import           Control.Applicative
import qualified Data.Text              as T
import           LambdaHive.Parser.Move
import           SlackBot.Types
import           Text.Trifecta
import           Web.Slack

slackBotCommandParser :: Parser SlackBotCommand
slackBotCommandParser = userIdParser
  *> (string ":"
    <* optional (char ' ')
    *> choice [try endGameParser, try makeMoveParser, try showGameParser, newGameParser])
  where
    newGameParser = string "start " *> (try aiGameParser <|> humanGameParser)
    humanGameParser = StartHumanGame <$> playerParser
    makeMoveParser = MakeMove <$> moveParser
    aiGameParser = StartAIGame <$> playerParser <*> (char ' ' *> userIdParser)
    endGameParser = EndGame <$ string "end"
    showGameParser = ShowGame <$ string "show"

userIdParser :: Parser UserId
userIdParser = Id . T.pack <$> between (string "<@") (char '>') p
  where
    p = head <$> sepBy1 (some alphaNum) (char '|')
