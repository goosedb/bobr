module Bobr.QuickStart.Interface.Implicit (
  module Bobr.QuickStart.Interface.Implicit,
  I.withLogger,
  I.withLabel,
  I.withLabels,
  I.pushNamespace,
  (G.|>),
) where

import Data.Text.Builder.Linear qualified as T
import Data.Time (UTCTime)
import Bobr.Logger.General qualified as G
import Bobr.Logger.Interface.Implicit qualified as I
import Bobr.Severity (Severity (..))

type WithLogger m = I.WithLogger UTCTime Severity m

debug, info, notice, warn, err, critical, alert, emergency :: (WithLogger m, Monad m) => T.Builder -> m ()
debug = I.logMsg Debug
info = I.logMsg Info
notice = I.logMsg Notice
warn = I.logMsg Warning
err = I.logMsg Error
critical = I.logMsg Critical
alert = I.logMsg Alert
emergency = I.logMsg Emergency
