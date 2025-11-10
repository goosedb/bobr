{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE StrictData #-}

module Bobr.Logger.General where

import Control.Monad (when)
import Data.Aeson qualified as J
import Data.ByteString.Builder qualified as Bytes
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Text qualified as T
import Data.Text.Builder.Linear qualified as B

type PutLog m = Bytes.Builder -> m ()

data Label = Label
  { key :: T.Text
  , value :: J.Value
  }
  deriving (Eq, Ord, Show)

(|>) :: (J.ToJSON a) => T.Text -> a -> Label
(|>) k v = Label k (J.toJSON v)

class ToLabel a where
  toLabel :: a -> Label

instance ToLabel Label where
  toLabel = id

class ToLabels a where
  toLabels :: a -> [Label]

instance ToLabel a => ToLabels [a] where 
  toLabels = map toLabel

data LoggerHandleSettings n l m time severity = LoggerHandleSettings
  { putLog :: Namespace n -> l -> B.Builder -> time -> severity -> m ()
  , renderNamespace :: n -> [T.Text] -> n
  , renderLabels :: l -> [Label] -> l
  }

data Namespace r = Namespace {rendered :: r, source :: Seq T.Text}

data LoggerHandleContext n l = LoggerHandleContext
  { namespace :: Namespace n
  , labels :: l
  }

data LoggerHandle m time severity where
  LoggerHandle ::
    { settings :: LoggerHandleSettings rdn rdl m time severity
    , context :: LoggerHandleContext rdn rdl
    } ->
    LoggerHandle m time severity

{-# INLINEABLE addLabels #-}
addLabels :: [Label] -> LoggerHandle m t s -> LoggerHandle m t s
addLabels ls LoggerHandle{context = LoggerHandleContext{..}, ..} =
  LoggerHandle{context = LoggerHandleContext{labels = settings.renderLabels labels ls, ..}, ..}

{-# INLINEABLE pushNamespace #-}
pushNamespace :: [T.Text] -> LoggerHandle m t s -> LoggerHandle m t s
pushNamespace ns LoggerHandle{context = LoggerHandleContext{..}, ..} =
  LoggerHandle
    { context =
        LoggerHandleContext
          { namespace =
              Namespace
                { rendered = settings.renderNamespace namespace.rendered ns
                , source = namespace.source <> Seq.fromList ns
                }
          , ..
          }
    , ..
    }

{-# INLINE writeLog #-}
writeLog :: LoggerHandle m time severity -> severity -> time -> B.Builder -> m ()
writeLog LoggerHandle{..} s t m = settings.putLog context.namespace context.labels m t s

setMinLevel :: (Ord severity, Applicative m) => severity -> LoggerHandle m time severity -> LoggerHandle m time severity
setMinLevel sev = setFilter \sev' _ -> sev' >= sev

setFilter :: (Applicative m) => (severity -> Seq T.Text -> Bool) -> LoggerHandle m time severity -> LoggerHandle m time severity
setFilter f LoggerHandle{settings = LoggerHandleSettings{..}, ..} =
  LoggerHandle{settings = LoggerHandleSettings{putLog = \n l b t s -> when (f s n.source) (putLog n l b t s), ..}, ..}
