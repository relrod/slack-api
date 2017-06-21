{-# LANGUAGE LambdaCase #-}

module Tests.ConnectionTest (main) where

import Control.Monad.IO.Class
import Data.Maybe
import System.Environment
import System.Exit
import Web.Slack
import qualified Web.Slack.Compat as C

main :: IO ()
main = do
    conf <- mkConfig
    withSlackHandle conf inertBot1
    C.runBot conf inertBot2 ()

mkConfig :: IO SlackConfig
mkConfig = do
    x <- lookupEnv "SLACK_API_TOKEN"
    let apiToken = fromMaybe (error "SLACK_API_TOKEN not set") x
    return SlackConfig{ _slackApiToken = apiToken }

inertBot1 :: SlackHandle -> IO ()
inertBot1 h =
    getNextEvent h >>= \case
        Hello -> return ()
        e -> error ("Unexpected event: " ++ show e)

inertBot2 :: C.SlackBot ()
inertBot2 e = case e of
    Hello -> liftIO exitSuccess
    _ -> error ("Unexpected event: " ++ show e)
