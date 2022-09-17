{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TFBoard where

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
import Types

class ScoreB m where
  registerPointTf :: SidePoint b -> m ()
  currentScoreTf :: m ScoreBoard
