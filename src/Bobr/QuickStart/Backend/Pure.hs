{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE ViewPatterns #-}

module Bobr.QuickStart.Backend.Pure where

import Control.Monad.Writer (MonadWriter (tell))
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Text.Builder.Linear qualified as B
import Bobr.Logger.General (Label, LoggerHandle (..), LoggerHandleContext (..), LoggerHandleSettings (..))
import qualified Data.Sequence as Seq

data Message time severity = Message
  { namespace :: Seq Text
  , labels :: Seq Label
  , time :: time
  , message :: Text
  , severity :: severity
  }
  deriving (Eq, Ord, Show)

withPureLogger ::
  ( MonadWriter (f (Message time severity)) m
  , forall x. Monoid (f x)
  , Applicative f
  ) =>
  (LoggerHandle m time severity -> m a) ->
  m a
withPureLogger action =
  action
    LoggerHandle
      { settings =
          LoggerHandleSettings
            { putLog = \namespace labels (B.runBuilder -> message) time severity -> tell (pure Message{..})
            , renderNamespace = \a b -> a <> Seq.fromList b
            , renderLabels = \a b -> a <> Seq.fromList b
            }
      , context = LoggerHandleContext mempty mempty
      }
