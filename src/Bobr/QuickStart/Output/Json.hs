module Bobr.QuickStart.Output.Json where

import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as J
import Data.Aeson.Key qualified as J
import Data.Foldable qualified as F
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as T
import Data.Time (UTCTime)
import Bobr.Logger.General (Label (..), LoggerHandle (..), LoggerHandleContext (LoggerHandleContext), LoggerHandleSettings (..), PutLog, Namespace (..))
import Bobr.Severity (Severity, severityToText)
import qualified Data.Text.Encoding as Text.Encode
import qualified Data.ByteString.Builder.Prim as Builder
import Data.Word (Word8)
import Data.Char (ord)
import qualified Data.ByteString.Builder as Bytes.Builder
import Data.Maybe (fromMaybe)

data ContextLocation = InsideObject J.Key | TopLevel

data JsonLoggerConfig time severity = JsonLoggerConfig
  { formatTime :: time -> J.Series
  , formatSeverity :: severity -> J.Series
  , contextLocation :: ContextLocation
  }

defaultJsonLoggerConfig :: ContextLocation -> JsonLoggerConfig UTCTime Severity
defaultJsonLoggerConfig loc =
  JsonLoggerConfig
    { formatTime = J.pair "timestamp" . J.utcTime
    , formatSeverity = J.pair "severity" . J.text . severityToText
    , contextLocation = loc
    }

mkJsonLogger :: JsonLoggerConfig time severity -> PutLog IO -> LoggerHandle IO time severity
mkJsonLogger JsonLoggerConfig{..} logAction =
  let attachMessage (Namespace rdn _) rdl msg stamp severity =
        let message =
              J.fromEncoding . J.pairs $
                J.pair "message" (J.text $ T.runBuilder msg)
                  <> formatTime stamp
                  <> formatSeverity severity
                  <> maybe mempty (J.pair "namespace" .  J.unsafeToEncoding . ("\"" <>) . (<> "\"")) rdn
                  <> case contextLocation of
                    InsideObject key -> J.pair key (J.pairs $ fromMaybe mempty rdl)
                    TopLevel -> fromMaybe mempty rdl
         in message <> "\n"
   in LoggerHandle
        { settings =
            LoggerHandleSettings
              { putLog = \namespace labels msg stamp level -> logAction $ attachMessage namespace labels msg stamp level
              , renderLabels = \old new -> 
                  let new' = foldMap (\Label{..} ->  J.pair (J.fromText key) (J.value value)) new
                  in Just $ maybe new' (<> new') old 
              , renderNamespace = \old n -> 
                  let new = Text.intercalate "." (F.toList n)
                  in if Text.null new then old else Just $ maybe (quote new) (\old' -> old' <> "." <> quote new) old
              }
        , context = LoggerHandleContext (Namespace Nothing mempty) Nothing 
        }

quote :: Text.Text -> Bytes.Builder.Builder
quote = Text.Encode.encodeUtf8BuilderEscaped escapeAscii


{-# INLINE escapeAscii #-}
escapeAscii :: Builder.BoundedPrim Word8
escapeAscii =
    Builder.condB (== c2w '\\'  ) (ascii2 ('\\','\\')) $
    Builder.condB (== c2w '\"'  ) (ascii2 ('\\','"' )) $
    Builder.condB (>= c2w '\x20') (Builder.liftFixedToBounded Builder.word8) $
    Builder.condB (== c2w '\n'  ) (ascii2 ('\\','n' )) $
    Builder.condB (== c2w '\r'  ) (ascii2 ('\\','r' )) $
    Builder.condB (== c2w '\t'  ) (ascii2 ('\\','t' )) $
    Builder.liftFixedToBounded hexEscape
  where
    hexEscape :: Builder.FixedPrim Word8
    hexEscape = (\c -> ('\\', ('u', fromIntegral c))) Builder.>$<
        Builder.char8 Builder.>*< Builder.char8 Builder.>*< Builder.word16HexFixed

    c2w :: Char -> Word8
    c2w c = fromIntegral (ord c)
    {-# INLINE c2w #-}

    ascii2 :: (Char, Char) -> Builder.BoundedPrim a
    ascii2 cs = Builder.liftFixedToBounded $ const cs Builder.>$< Builder.char7 Builder.>*< Builder.char7
    {-# INLINE ascii2 #-}