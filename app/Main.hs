{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import           Control.Concurrent                   as C
import           Control.Concurrent.STM               (STM (..))
import qualified Control.Concurrent.STM               as S
import           Control.Monad
import           Data.Map                             (Map (..))
import qualified Data.Map                             as M
import           Graphics.Gloss                       as G
import           Graphics.Gloss.Data.Color            as G
import           Graphics.Gloss.Interface.IO.Simulate as G
import           System.Random                        as R
import           VTVar

import           Data.Maybe                           (fromJust)

data Ant = Ant
  { antPos       :: VTVar G.Point
  , antHealth    :: VTVar Int
  , antDirection :: VTVar G.Vector
  , antState     :: ObjectState
  }

type AntV = VTVar Ant

data ObjectState
    = ObjectAlive
    | ObjectDead
    deriving Eq

data Object
  = OAnt
  | OFood
  | OPheromon

data Home = Home
    { hPos  :: Point
    , hSize :: (Float, Float)
--    , mFoodEaten:: VTVar Int
    }

-- newtype Grid = Map (Int, Int) Object

type HomeV = VTVar Home

data World = World
    { wStdGen :: RandomV -- random generator
    , wHome   :: HomeV -- ants' home

--    , wAnts:: VTVar [ AntV ]
--    , wPheromons:: VTVar [ G.Point ]
--    , wFood:: VTVar [ G.Point]
    }

type WorldV = VTVar World

type RandomV = VTVar StdGen


mkRandomV :: IO RandomV
mkRandomV = do
    stdgen <- getStdGen
    runWSTM $ atomicallyW $ newVTVar stdgen

mkHomeV :: RandomV-> IO HomeV
mkHomeV rndV = fromJust . snd <$> rcuVTVar read calc write
  where
    read :: RSTM (RandomV, VTVarI StdGen)
    read = do
      gI <- atomicallyR $ readVTVar rndV
      return ( rndV, gI)
    calc:: (RandomV, VTVarI StdGen)-> ((), Maybe ((RandomV, VTVarI StdGen),Home))
    calc (g, (g0, gV)) =
      let (newPos,g1) = random g0
          home = Home
            { hPos = newPos
            , hSize = (40.0, 40.0)
            }
      in ((), Just ((g, (g1, gV)),home))
    write ((g, gI), home) = do
      writeVTVar g gI
      newVTVar home



instance Random Point where
    randomR (( minx, miny), ( maxx, maxy)) g =
      let
          (newx, stdgen0) = randomR (minx, maxx) g
          (newy, stdgen1) = randomR (miny, maxy) stdgen0
      in ((newx, newy), stdgen1)
    random g = let
      hw = fromIntegral width / 2
      hh = fromIntegral height / 2
      (newx, g0) = randomR (-hw, hw) g
      (newy, g1) = randomR (-hh, hh) g0
      in ((newx, newy), g1)

main = do
  initialState <- makeState
  G.simulateIO display background 30 initialState drawIO update
  where
    display = G.InWindow "Ants Colony" (width, height) (50, 50)
    background  = G.white
    update _ _ = return

width, height::Int
width = 800
height = 600

drawIO
    :: WorldV
    -> IO Picture
drawIO worldV = G.pictures <$> sequence
    [ return drawAxis
    , drawHome worldV
    ]

drawAxis :: Picture
drawAxis = G.color G.black
    $ G.pictures
    [ G.line [(0, height'/2), (0, (-height')/2)]
    , G.line [((-width')/2, 0), (width'/2, 0)]
    ]
    where
    width' = fromIntegral width
    height' = fromIntegral height

{- do
    state <- initialState
    gloss state update input render

initialState:: IO WorldV
initialState = do
    ants <- newVTVar V.empty
    pheromons <- newVTVar V.empty
    foods <- newVTVar V.empty
    let mother = Mother (0,0)
    let world = World mother ants pheromons food
    newVTVar $ world
-}

makeState :: IO WorldV
makeState = do
    stdgenV <- mkRandomV
    homeV <- mkHomeV stdgenV
    runWSTM $ atomicallyW $ newVTVar World
      { wStdGen = stdgenV
      , wHome = homeV
      }

drawHome
  :: WorldV
  -> IO Picture
drawHome worldT = fst <$> rcuVTVar readVar calc ( const $ return ())
  where
    readVar :: RSTM Home
    readVar = atomicallyR $ do
        (world, _) <- readVTVar worldT
        (home, _ ) <- readVTVar $ wHome world
        return home
    calc :: Home-> (Picture, Maybe ())
    calc home = (payload, Nothing)
      where
        payload = G.pictures
          [ G.line [ (x0,y0), (x1, y0), (x1,y1), (x0, y1), (x0, y0)]
          ]
        (x0, y0) = hPos home
        (w,h) = hSize home
        (x1,y1) = (x0 + w, y0 + h)
