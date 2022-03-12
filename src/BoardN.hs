{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
module BoardN where

import Control.Concurrent (threadDelay)
import Control.Monad
-- import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import GHC.Generics (K1(K1))

import Data.Map (Map)
import Data.Map qualified as Map
import Control.Concurrent.STM (TVar, readTVarIO, newTVarIO, stateTVar)
import Control.Monad.STM (atomically)


-- Model --
data Point
  = H
  | K
  | U
  | W
  deriving stock (Show, Eq, Ord, Enum)

data Side
  = White
  | Blue
  deriving stock (Show)

data SideScore = SideScore
  { side :: Side -- TODO: More safety with data kinds?
  , score :: Map Point Int -- TODO: Natural
  }
  deriving stock (Show)

addToScore :: Point -> SideScore -> SideScore
addToScore pt ss = ss {score = newScore}
  where
    newScore = Map.update (Just . succ) pt (score ss)

emptySide side' = SideScore side' (Map.fromList emptyMap)
  where emptyMap = zip [H .. ] (repeat 0)

data ScoreBoard = ScoreBoard
  { whiteSide :: SideScore
  , blueSide :: SideScore
  }
  deriving stock (Show)

emptyBoard :: ScoreBoard
emptyBoard =
  ScoreBoard
    (emptySide White)
    (emptySide Blue)

addPoint :: Side -> Point -> ScoreBoard -> ScoreBoard
addPoint side' pt' sb = case side' of
  White -> sb { whiteSide = addToScore pt' (whiteSide sb)} -- TODO: lenses?
  Blue -> sb { blueSide = addToScore pt' (blueSide sb)}

pointScore :: Side -> Point -> ScoreBoard -> Int
pointScore side' pt' sb' =
  case side' of
    White -> score (whiteSide sb') Map.! pt'
    Blue -> score (blueSide sb') Map.! pt'


-- UI --
someFunc :: IO ()
someFunc = do
  static <- getStaticDir
  newState <- newTVarIO emptyBoard
  startGUI defaultConfig {jsStatic = Just static} (setup newState)
  where
    getStaticDir = pure "static"

setup :: TVar ScoreBoard -> Window -> UI ()
setup sb w = void $ do
  pure w # set title "Board"
  getBody w #+
   [row  (uiWhiteSide sb)]

uiWhiteSide :: TVar ScoreBoard -> [UI Element]
uiWhiteSide sbT =
  (\pt -> mkPointTile White pt sbT) <$> [H ..]

mkPointTile :: Side -> Point -> TVar ScoreBoard -> UI Element
mkPointTile side' pt' sbT = do
  sb <- liftIO $ readTVarIO  sbT
  button <- UI.button #. "button" #+ [string "+"]
  nameLbl <- UI.label #. "point_lbl" #+ [string (show pt')]

  let 
  scoreLbl <- UI.label #. "label 1" #+ [string (show $ currentScore sb)]
  view <- UI.div #+
            [ UI.p #+ [element nameLbl]
            , UI.p #+ [element scoreLbl]
            , UI.p #+ [element button]
            ]
  on UI.click button $ \_ -> do
    newScore <- liftIO $ atomically $ stateTVar sbT f 
    element scoreLbl # set text (show newScore)
  return view
  where
    f sb' = let newSb = addPoint side' pt' sb'
            in (currentScore newSb, newSb)
    currentScore = pointScore side' pt'
