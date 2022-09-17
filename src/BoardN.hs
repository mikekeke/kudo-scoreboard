{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}

module BoardN where

import Control.Concurrent (threadDelay)
import Control.Monad
-- import Paths

-- import qualified Graphics.UI.Threepenny as UI
-- import Graphics.UI.Threepenny.Core

import Data.Map (Map)
import Data.Map qualified as Map
import Control.Concurrent.STM (TVar, readTVarIO, newTVarIO, stateTVar, modifyTVar', swapTVar)
import Control.Monad.STM (atomically)
import Data.Foldable (for_)
import Data.Traversable (for)
import Text.Show.Pretty (ppShow)
import Text.Read
import  System.Environment
import Types

-- app
newtype App = App (TVar ScoreBoard)

newApp :: IO App
newApp = App <$> newTVarIO emptyBoard

registerPoint :: SidePoint a -> App -> IO ScoreBoard
registerPoint sp (App st) = do
  sb <- readTVarIO st
  let newBoard = addPoint sp sb
  atomically $ swapTVar st newBoard
  pure newBoard

-- UI --
someFunc :: IO ()
someFunc = do
  app <- newApp
  sb <- registerPoint (WhitePoint U) app
  putStrLn $ ppShow  sb
  sb <- registerPoint (BluePoint W) app
  putStrLn $ ppShow  sb

  -- forever $ do
  --   s <- getLine
  --   p <- getLine
  --   case (readMaybe s, readMaybe p) of
  --     (Just s', Just p') -> do
  --       sb <- pointTo s' p' app
  --       putStrLn $ ppShow  sb
  --     e -> putStrLn $ "Unknown side or point: " ++ show s ++ " | " ++ show p


