{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}

module Bobr.Logger.Interface.Monad where

import Data.Text (Text)
import Data.Text.Builder.Linear qualified as T
import Bobr.Logger.General (Label)
import Bobr.Logger.General qualified as G

class (MonadLoggerTime time m) => MonadLogger time severity m | m -> time, m -> severity where
  getLogger :: m (G.LoggerHandle m time severity)
  localLogger :: (G.LoggerHandle m time severity -> G.LoggerHandle m time severity) -> m a -> m a

class (Monad m) => MonadLoggerTime time m where
  getTime :: m time

pushNamespace :: forall time severity m a. (MonadLogger time severity m) => [Text] -> m a -> m a
pushNamespace ns = localLogger @time @severity (G.pushNamespace ns)

withLabels :: forall time severity m a. (MonadLogger time severity m) => [G.Label] -> m a -> m a
withLabels labels = localLogger @time @severity (G.addLabels labels)

withLabel :: forall time severity m a. (MonadLogger time severity m) => Label -> m a -> m a
withLabel l = withLabels @time @severity [l]

logMsg :: forall time severity m. (MonadLogger time severity m) => severity -> T.Builder -> m ()
logMsg sev msg = do
  now <- getTime @time
  l <- getLogger
  G.writeLog l sev now msg
