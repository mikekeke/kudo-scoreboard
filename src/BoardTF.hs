{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module BoardTF where

import Control.Concurrent (threadDelay)
import Control.Monad
-- import Paths

-- import qualified Graphics.UI.Threepenny as UI
-- import Graphics.UI.Threepenny.Core

import Data.Map (Map)
import Data.Map qualified as Map
-- import Control.Concurrent.STM (TVar, readTVarIO, newTVarIO, stateTVar, modifyTVar', swapTVar)
import Control.Monad.STM (atomically)
import Data.Foldable (for_)
import Data.Traversable (for)
import Text.Show.Pretty (ppShow)
-- import Text.Read
import  System.Environment
import Data.IORef
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import TFBoard
import Types
import Control.Monad.IO.Class


newtype TfApp a = TfApp {runApp :: ReaderT (IORef ScoreBoard) IO a} 
  deriving newtype (Functor, Applicative, Monad, MonadIO)

runTfApp ior tfa = runReaderT (runApp tfa) ior 

instance ScoreB TfApp where
  -- registerPoint sp = TfApp . ReaderT $ \spRef -> do
  --   modifyIORef spRef (addPoint sp)

  registerPointTf sp = TfApp $ do
    spRef <- ask
    lift $ modifyIORef spRef (addPoint sp)

  currentScoreTf = TfApp $ do
    spRef <- ask
    lift $ readIORef spRef