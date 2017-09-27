{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Web.Slack.Types.Item where

import Data.Aeson
import Web.Slack.Types.Id
import Web.Slack.Types.File
import Web.Slack.Types.Comment
import Web.Slack.Types.Time
import Control.Applicative
import Data.Text (Text)
import Web.Slack.Types.Base

import Control.Lens.TH
import Prelude

data Item = MessageItem ChannelId MessageUpdate
          | FileItem File
          | FileCommentItem File Comment
          | ChannelItem ChannelId
          | IMItem ChannelId
          | GroupItem ChannelId deriving Show

data EmbeddedItem
          = EmbeddedMessageItem ChannelId SlackTimeStamp
          | EmbeddedFileItem FileId
          | EmbeddedFileCommentItem FileId CommentId
          deriving Show

instance  FromJSON Item where
  parseJSON = withObject "item" (\o -> do
                (typ :: String) <- o .: "type"
                case typ of
                  "message" -> MessageItem <$> o .: "channel" <*> o .: "message"
                  "file" -> FileItem <$> o .: "file"
                  "file_comment" -> FileCommentItem <$> o .: "file" <*> o .: "comment"
                  "channel" -> ChannelItem <$> o .: "channel"
                  "im"      -> IMItem <$> o .: "channel"
                  "group"   -> GroupItem <$> o .: "group"
                  _         -> fail $ "Unrecognised item type: " ++ typ)

instance FromJSON EmbeddedItem where
  parseJSON = withObject "item" $ \o -> do
                (typ :: String) <- o .: "type"
                case typ of
                  "message"      -> EmbeddedMessageItem <$> o .: "channel" <*> o .: "ts"
                  "file"         -> EmbeddedFileItem <$> o .: "file"
                  "file_comment" -> EmbeddedFileCommentItem <$> o .: "file" <*> o .: "file_comment"
                  _              -> fail $ "Unrecognised item type: " ++ typ

data MessageUpdate = MessageUpdate
                   { _messageUpdateUser   :: UserId
                   , _messageUpdateText   :: Text
                   , _messageUpdateTime   :: SlackTimeStamp
                   , _messageUpdateEdited :: Maybe Edited
                   , _messagePermalink    :: Maybe URL
                   } deriving Show

instance FromJSON MessageUpdate where
  parseJSON = withObject "MessageUpdate"
                (\o -> MessageUpdate <$> o .: "user"
                        <*> o .: "text" <*> o .: "ts"
                        <*> o .:? "edited" <*> o .:? "permalink" )

data Edited = Edited { _editedUser :: UserId, _editTimestap :: SlackTimeStamp } deriving Show

newtype ReplyCount = ReplyCount Integer deriving (Show, Num, Eq, Ord)

data MessageReplied = MessageReplied
                    { _messageRepliedUser :: UserId
                    , _messageRepliedText :: Text
                    , _messageRepliedTime :: SlackTimeStamp
                    , _messageRepliedReplyCount :: ReplyCount
                    , _messageRepliedReplies :: [Reply]
                    } deriving Show

data Reply = Reply
           { _replyUser :: UserId
           , _replyTime :: SlackTimeStamp
           } deriving Show

instance FromJSON MessageReplied where
  parseJSON = withObject "MessageReplied"
                (\o -> MessageReplied <$> o .: "user"
                        <*> o .: "text" <*> o .: "ts"
                        <*> o .: "reply_count" <*> o .: "replies" )

instance FromJSON ReplyCount where
  parseJSON v = ReplyCount <$> parseJSON v

instance FromJSON Reply where
  parseJSON = withObject "Reply" (\o -> Reply <$> o .: "user" <*> o .: "ts")

makeLenses ''MessageUpdate
makeLenses ''Edited
makeLenses ''MessageReplied
makeLenses ''Reply

instance FromJSON Edited where
  parseJSON = withObject "Edited" (\o -> Edited <$> o .: "user" <*> o .: "ts")
