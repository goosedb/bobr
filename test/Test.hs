{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (Reader, ReaderT (runReaderT))
import Control.Monad.Writer.Strict (Writer, runWriter)
import Data.Aeson qualified as J
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.ByteString.Builder qualified as Bytes
import Data.Functor.Identity (Identity (runIdentity))
import Data.IORef (IORef, modifyIORef, modifyIORef', newIORef, readIORef, atomicModifyIORef')
import Data.Maybe (fromJust)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Time (UTCTime (UTCTime), addUTCTime, fromGregorian, getCurrentTime)
import Bobr.Logger.General (LoggerHandle, pushNamespace, (|>), PutLog)
import Bobr.Logger.Interface.Implicit qualified as I
import Bobr.Logger.Interface.Monad (MonadLogger (..), MonadLoggerTime (getTime), pushNamespace)
import Bobr.QuickStart.Backend.Pure (Message (..), withPureLogger)
import Bobr.QuickStart.Interface.Implicit qualified as QI
import Bobr.QuickStart.Interface.Monad qualified as M
import Bobr.QuickStart.Output.Console (mkConsoleLogger)
import Bobr.QuickStart.Output.Json (ContextLocation (..), JsonLoggerConfig (..), defaultJsonLoggerConfig, mkJsonLogger)
import Bobr.Severity (Severity (..))
import Test.Hspec
import Bobr.QuickStart.Backend.Async (withAsyncPutLog)
import Control.Concurrent (threadDelay, yield)
import Data.Foldable (for_)
import GHC.IO (throwIO)
import Control.Exception (AsyncException(UserInterrupt), handle, try)
import Bobr.QuickStart.Backend.Async (Worker, killBackgroundLogger)
import GHC.Exts (IsString(..))
import GHC.Conc (newTVarIO, TVar, atomically, readTVar, writeTVar, readTVarIO, registerDelay)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as Text
import Data.Bool (bool)
import GHC.Conc.Sync (retry)

type Msg = Message () Severity

msg :: Message () Severity
msg =
  Message
    { namespace = mempty
    , labels = mempty
    , time = ()
    , message = ""
    , severity = Info
    }

jsonHandle :: PutLog IO  -> LoggerHandle IO UTCTime Severity
jsonHandle = mkJsonLogger (defaultJsonLoggerConfig $ InsideObject "data") 

putLogRef :: TVar (Seq p) -> p -> IO ()
putLogRef ref l = atomically $ readTVar ref >>= writeTVar ref . (Seq.|> l) 

timer :: IO (IO UTCTime)
timer = do
  timeRef <- newIORef (UTCTime (fromGregorian 2025 01 01) 0)
  pure $ readIORef timeRef <* modifyIORef timeRef (addUTCTime 1)

parseLogs :: Seq Bytes.Builder -> Seq J.Value
parseLogs = fmap (fromJust . J.decode @J.Value . Bytes.toLazyByteString)

runJsonLogger :: ((QI.WithLogger IO) => IO a) -> IO (Seq J.Value)
runJsonLogger action = do
  ref <- newTVarIO []
  getTime <- timer
  _ <- I.withLogger (jsonHandle $ putLogRef ref) getTime action
  parseLogs <$> readTVarIO ref


runPureLogger :: ((I.WithLogger () Severity (Writer (Seq Msg))) => Writer (Seq Msg) ()) -> Seq Msg
runPureLogger action = snd $ runWriter $ withPureLogger \l -> I.withLogger l (pure ()) action

main :: IO ()
main = hspec do
  describe "format" do
    let
      scenario :: (QI.WithLogger IO) => IO ()
      scenario = do
        QI.debug "hello"
        QI.pushNamespace ["foo"] do
          QI.info "aloha"
          QI.pushNamespace ["bar"] do
            QI.info "salut"
          QI.withLabels ["bar" |> True, "baz" |> 'a'] do
            QI.info "привет"

    it "json" do
      logs <- runJsonLogger scenario
      logs
        `shouldBe` [ [aesonQQ| { "data": {}, "message": "hello", "severity": "Debug", "timestamp": "2025-01-01T00:00:00Z" } |]
                   , [aesonQQ| { "data": {}, "message": "aloha", "severity": "Info", "timestamp": "2025-01-01T00:00:01Z", "namespace": "foo" } |]
                   , [aesonQQ| { "data": {}, "message": "salut", "severity": "Info", "timestamp": "2025-01-01T00:00:02Z", "namespace": "foo.bar" } |]
                   , [aesonQQ| { "data": { "bar": true, "baz": "a" }, "message": "привет", "severity": "Info", "timestamp": "2025-01-01T00:00:03Z", "namespace": "foo" } |]
                   ]
    describe "pure" do
      it "namespace" do
        let logs = runPureLogger do
              I.logMsg Debug "hello"
              I.pushNamespace ["foo"] do
                I.logMsg Warning "привет"
                I.pushNamespace ["bar"] do
                  I.logMsg Error "aloha"
        logs
          `shouldBe` [ msg{message = "hello", severity = Debug}
                     , msg{message = "привет", namespace = ["foo"], severity = Warning}
                     , msg{message = "aloha", namespace = ["foo", "bar"], severity = Error}
                     ]
      it "labels" do
        let logs = runPureLogger do
              I.logMsg Debug "hello"
              I.withLabel ("foo" |> True) do
                I.logMsg Warning "привет"
                I.withLabels ["bar" |> 'a', "baz" |> ()] do
                  I.logMsg Error "aloha"
        logs
          `shouldBe` [ msg{message = "hello", severity = Debug}
                     , msg{message = "привет", labels = ["foo" |> True], severity = Warning}
                     , msg{message = "aloha", labels = ["foo" |> True, "bar" |> 'a', "baz" |> ()], severity = Error}
                     ]
      it "labels & namespace" do
        let logs = runPureLogger do
              I.logMsg Debug "hello"
              I.pushNamespace ["foo"] do
                I.logMsg Notice "salut"
                I.withLabel ("foo" |> True) do
                  I.logMsg Warning "привет"
                  I.pushNamespace ["bar"] do
                    I.withLabels ["bar" |> 'a', "baz" |> ()] do
                      I.logMsg Error "aloha"
        logs
          `shouldBe` [ msg{message = "hello", severity = Debug}
                     , msg{message = "salut", namespace = ["foo"], severity = Notice}
                     , msg{message = "привет", namespace = ["foo"], labels = ["foo" |> True], severity = Warning}
                     , msg{message = "aloha", namespace = ["foo", "bar"], labels = ["foo" |> True, "bar" |> 'a', "baz" |> ()], severity = Error}
                     ]
