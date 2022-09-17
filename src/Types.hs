{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Foldable (for_)
import Data.Traversable (for)
import Text.Show.Pretty (ppShow)


-- Model --
data Point
  = H
  | K
  | U
  | W
  deriving stock (Show, Eq, Ord, Enum, Read)

data Side
  = White
  | Blue
  deriving stock (Show, Eq, Ord, Read)

data SidePoint (a :: Side) where
  WhitePoint :: Point -> SidePoint White
  BluePoint :: Point -> SidePoint Blue

getPoint :: SidePoint a -> Point
getPoint = \case
  WhitePoint p -> p
  BluePoint p -> p

data SideScore (a :: Side) = SideScore
  { score :: Map Point Int -- TODO: Natural
  }
  deriving stock (Show)

addToScore :: SidePoint a -> SideScore a -> SideScore a
addToScore pt ss = ss {score = newScore}
  where
    newScore = Map.update (Just . succ) (getPoint pt) (score ss)

emptySide = SideScore (Map.fromList emptyMap)
  where emptyMap = zip [H .. ] (repeat 0)

data ScoreBoard = ScoreBoard
  { whiteSide :: SideScore White
  , blueSide :: SideScore Blue
  }
  deriving stock (Show)

emptyBoard :: ScoreBoard
emptyBoard =
  ScoreBoard
    emptySide
    emptySide

addPoint :: SidePoint a -> ScoreBoard -> ScoreBoard
addPoint sp sb = case sp of
  WhitePoint p -> sb { whiteSide = addToScore sp (whiteSide sb)}
  BluePoint p -> sb { blueSide = addToScore sp (blueSide sb)}
