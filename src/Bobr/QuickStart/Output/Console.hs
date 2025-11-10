{-# LANGUAGE ScopedTypeVariables #-}

module Bobr.QuickStart.Output.Console where

import Bobr.Logger.General (Label (..), LoggerHandle (..), LoggerHandleContext (LoggerHandleContext), LoggerHandleSettings (..), Namespace (..), PutLog)
import Bobr.Severity (Severity (..), severityToText)
import Control.Applicative ((<|>))
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Data.Aeson.KeyMap qualified as J
import Data.Aeson.Text qualified as J
import Data.ByteString.Builder qualified as Bytes
import Data.Foldable (Foldable (..))
import Data.Foldable qualified as F
import Data.Functor ((<&>))
import Data.List (intersperse)
import Data.Text qualified as T
import Data.Text qualified as Text
import Data.Text.Builder.Linear qualified as B
import Data.Text.Builder.Linear qualified as Text.Linear
import Data.Text.Lazy qualified as TL
import Data.Time (getCurrentTimeZone)
import Data.Time qualified as Time
import System.Console.ANSI (Color (..), ColorIntensity (..), ConsoleLayer (..), SGR (..), setSGRCode)

data ConsoleLoggerConfig time severity = ConsoleLoggerConfig
  { severityColor :: severity -> [SGR]
  , timestampColor :: [SGR]
  , namespaceColor :: [SGR]
  , labelKeyColor :: [SGR]
  , labelValueColor :: [SGR]
  , formatTime :: time -> Maybe B.Builder
  , formatSeverity :: severity -> B.Builder
  }

defaultConsoleLoggerConfig :: (MonadIO m) => String -> m (ConsoleLoggerConfig Time.UTCTime Severity)
defaultConsoleLoggerConfig timestampFormat = liftIO do
  timezome <- getCurrentTimeZone
  let timeFormat = Just . utcTimeFormat timezome timestampFormat
  pure $ ConsoleLoggerConfig severityColor timestampColor namespaceColor labelKeyColor labelValueColor timeFormat showSeverity
 where
  namespaceColor = [SetColor Foreground Dull Magenta]
  timestampColor = [SetColor Foreground Vivid White]
  labelKeyColor = [SetColor Foreground Vivid Blue]
  labelValueColor = [SetColor Foreground Vivid Blue]
  showSeverity = Text.Linear.fromText . severityToText
  severityColor =
    pure . uncurry (SetColor Foreground) . \case
      Debug -> (Dull, Black)
      Info -> (Vivid, Green)
      Notice -> (Vivid, Green)
      Warning -> (Vivid, Yellow)
      Error -> (Vivid, Red)
      Critical -> (Vivid, Red)
      Alert -> (Vivid, Red)
      Emergency -> (Vivid, Red)
  utcTimeFormat tz format = B.fromText . T.pack . Time.formatTime Time.defaultTimeLocale format . Time.utcToLocalTime tz

mkConsoleLogger :: forall time severity. ConsoleLoggerConfig time severity -> PutLog IO -> LoggerHandle IO time severity
mkConsoleLogger ConsoleLoggerConfig{..} logAction =
  let
    attachMessage :: Namespace (Maybe B.Builder) -> Maybe B.Builder -> B.Builder -> time -> severity -> Bytes.Builder
    attachMessage (Namespace rdn _) rdl msg stamp severity =
      Bytes.byteString . B.runBuilderBS $
        fold
          [ maybe "" (\t -> printTimestamp $ "[" <> t <> "]") (formatTime stamp)
          , " "
          , printSeverity severity
          , maybe "" (" " <>) rdn
          , maybe "" (" " <>) rdl
          , " "
          , msg
          , "\n"
          ]
   in
    LoggerHandle
      { settings =
          LoggerHandleSettings
            { putLog = \namespace labels msg stamp level ->
                logAction $ attachMessage namespace labels msg stamp level
            , renderNamespace = \old ->
                \case
                  [] -> old
                  n ->
                    let new = (formatNamespace . B.fromText . Text.intercalate "/" $ F.toList n)
                     in Just $ maybe new (\old' -> old' <> "/" <> new) old
            , renderLabels = \old m ->
                let new =
                      if null m
                        then Nothing
                        else Just (fold . intersperse " " $ renderLabels m)
                 in case (old, new) of
                      (Just o, Just n) -> Just $ o <> " " <> n
                      (o, n) -> o <|> n
            }
      , context = LoggerHandleContext (Namespace Nothing mempty) Nothing
      }
 where
  renderValues :: Text.Text -> J.Value -> [(B.Builder, B.Builder)]
  renderValues path = \case
    J.String t -> [(B.fromText path, B.fromText t)]
    v@J.Number{} -> [(B.fromText path, toTextAsIs v)]
    v@J.Bool{} -> [(B.fromText path, toTextAsIs v)]
    J.Null -> []
    J.Object obj -> concatMap (\(k, v) -> renderValues (path <> "." <> J.toText k) v) $ J.toList obj
    J.Array arr -> concat $ zipWith (\i v -> renderValues (path <> "." <> Text.pack (show @Int i)) v) [0 ..] $ F.toList arr

  toTextAsIs = B.fromText . TL.toStrict . J.encodeToLazyText

  renderLabels =
    concat
      . F.toList
      . fmap
        ( \Label{..} ->
            let vals = renderValues key value
             in vals <&> \(k, v) -> "#" <> printKey k <> "=" <> printValue v
        )

  formatNamespace ns
    | null namespaceColor = ns
    | otherwise = format namespaceColor <> ns <> reset

  printSeverity :: severity -> B.Builder
  printSeverity sev =
    let color = severityColor sev
     in if null color
          then formatSeverity sev
          else format color <> "[" <> formatSeverity sev <> "]" <> reset

  formatTimestamp = format timestampColor
  printTimestamp ts
    | null timestampColor = ts 
    | otherwise = formatTimestamp <> ts <> reset

  formatKey = format labelKeyColor
  printKey k
    | null labelKeyColor = k
    | otherwise = formatKey <> k <> reset

  formatValue = format labelValueColor
  printValue v 
    | null labelValueColor = v
    | otherwise = formatValue <> v <> reset

  format :: [SGR] -> B.Builder
  format sgr = B.fromText $ T.pack (setSGRCode sgr)

  reset :: B.Builder
  reset = B.fromText $ T.pack $ setSGRCode [Reset]
