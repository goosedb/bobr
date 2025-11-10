module Bobr.Severity where

import Data.Text qualified as T

data Severity = Debug | Info | Notice | Warning | Error | Critical | Alert | Emergency
  deriving (Eq, Ord, Show, Read, Enum)

severityToText :: Severity -> T.Text
severityToText = \case
  Debug -> debugText
  Info -> infoText
  Warning -> warningText
  Error -> errorText
  Notice -> noticeText
  Critical -> criticalText
  Alert -> alertText
  Emergency -> emergencyText

debugText, infoText, warningText, errorText, noticeText, criticalText, alertText, emergencyText :: T.Text
debugText = "Debug"
infoText = "Info"
warningText = "Warning"
errorText = "Error"
noticeText = "Notice"
criticalText = "Critical"
alertText = "Alert"
emergencyText = "Emergency"
