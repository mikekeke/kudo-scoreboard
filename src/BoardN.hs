{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TupleSections #-}
module BoardN where

import Control.Concurrent (threadDelay)
import Control.Monad
-- import Paths

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import GHC.Generics (K1(K1))

import Data.Map (Map)
import Data.Map qualified as Map
import Control.Concurrent.STM (TVar, readTVarIO, newTVarIO, stateTVar, modifyTVar')
import Control.Monad.STM (atomically)
import Data.Foldable (for_)
import Data.Traversable (for)


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
  deriving stock (Show, Eq, Ord)

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
  boardView <- mkBoardView
  setClickers sb boardView
  getBody w #+
   [row  (toView boardView)]

setClickers :: TVar ScoreBoard -> BoardView -> UI ()
setClickers sbT bw = do
  let views = [ (side', pt', v) | (side', pm) <- Map.toList (unBoard bw), (pt', v) <- Map.toList pm]
  for_ views $ \(side', pt', tw) ->
      on UI.click (tvPlusBtn tw) $ \_ -> do
        liftIO $ atomically $ modifyTVar' sbT (addPoint side' pt')
        newSb' <- liftIO $ readTVarIO sbT
        renderBoard newSb' bw

uiWhiteSide :: TVar ScoreBoard -> [UI Element]
uiWhiteSide sbT = undefined
  -- (\pt -> mkPointTile White pt sbT) <$> [H ..]

mkBoardView :: UI BoardView
mkBoardView = do
  whiteSide <-
    Map.fromList <$> mapM (\pt -> (pt,) <$> mkPointTile pt) [H ..]

  return $ BoardView (Map.singleton White whiteSide)

mkPointTile :: Point -> UI TileView
mkPointTile pt' = do
  button <- UI.button #. "button" #+ [string "+"]
  nameLbl <- UI.label #. "point_lbl" #+ [string (show pt')]

  scoreLbl <- UI.label #. "label 1" #+ [string (show 0)]
  view <- UI.div #+
            [ UI.p #+ [element nameLbl]
            , UI.p #+ [element scoreLbl]
            , UI.p #+ [element button]
            ]
  return $ TileView nameLbl scoreLbl button

data TileView = TileView
  { tvTitle :: Element
  , tvScore :: Element
  , tvPlusBtn :: Element
  }

newtype BoardView = BoardView {unBoard :: Map Side (Map Point TileView) }

renderBoard :: ScoreBoard -> BoardView -> UI ()
renderBoard sb bv = do
  renderSide (whiteSide sb) bv
  -- renderSide (blueSide sb) bv -- TODO

renderSide :: SideScore -> BoardView -> UI ()
renderSide ss (BoardView boardMap) = do
  let sideView = boardMap Map.! side ss
  for_ (Map.toList $ score ss) $ \(pt, v) -> do
    let scoreLbl = tvScore $ sideView Map.! pt
    void $ element scoreLbl # set text (show v)

toView :: BoardView -> [UI Element]
toView (BoardView viewMap) =
  let whiteSide = viewMap Map.! White
  in flip map (Map.elems whiteSide) $ \(TileView t s btn) ->
    UI.div #+
      [ UI.p #+ [element t]
      , UI.p #+ [element s]
      , UI.p #+ [element btn]
      ]
