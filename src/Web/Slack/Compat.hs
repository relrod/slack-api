{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Compatibility with 0.12
--
-- These functions are included to aid backward-compatibility with version
-- 0.12 of this package.
--
-- > main :: IO ()
-- > main = runBot myConfig echobot ()
-- >
-- > echobot :: SlackBot ()
-- > echobot (Message cid _ msg _ _ _) = sendMessage cid msg
-- > echobot _ = return ()
module Web.Slack.Compat
    ( SlackBot
    , Slack
    , SlackState
    , handle
    , userState
    , runBot
    , sendMessage
    ) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Text as T
import qualified Web.Slack as H
import Web.Slack.Types

type SlackBot s = Event -> Slack s ()

newtype Slack s a = Slack
    { runSlack :: StateT (SlackState s) IO a
    } deriving (MonadState (SlackState s), MonadIO, Monad, Functor, Applicative)

data SlackState s = SlackState
    { _handle :: H.SlackHandle
    , _userState :: s
    }

makeLenses ''SlackState

runBot :: H.SlackConfig -> SlackBot s -> s -> IO ()
runBot conf runloop s0 =
    H.withSlackHandle conf $ \h ->
        flip evalStateT (SlackState h s0) $ forever $
            liftIO (H.getNextEvent h) >>= runSlack . runloop

sendMessage :: ChannelId -> T.Text -> Slack s ()
sendMessage cid msg = do
    h <- _handle <$> get
    liftIO $ H.sendMessage h cid msg
