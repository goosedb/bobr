{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}

module Bobr.QuickStart.Interface.Monad (
  module Bobr.QuickStart.Interface.Monad,
   M.withLabels,  M.withLabel,  M.pushNamespace,
  (G.|>),
)
where

import Data.Text.Builder.Linear qualified as T
import Data.Time (UTCTime)
import Bobr.Logger.General qualified as G
import Bobr.Logger.Interface.Monad as M
import Bobr.Severity (Severity (..))

debug, info, notice, warn, err, critical, alert, emergency :: (MonadLogger UTCTime Severity m) => T.Builder -> m ()
debug = logMsg Debug
info = logMsg Info
notice = logMsg Notice
warn = logMsg Warning
err = logMsg Error
critical = logMsg Critical
alert = logMsg Alert
emergency = logMsg Emergency
