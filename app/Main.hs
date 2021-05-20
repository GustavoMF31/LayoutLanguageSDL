{-# LANGUAGE OverloadedStrings, DataKinds #-}
module Main where

import Foreign.C.Types (CInt)

import qualified SDL
import SDL (V2(..), get, ($=))

import Widget --(Widget, WidgetSizeDependency(..), flexibleSquare, row, limit, alignLeft, alignTop)
import Drawable

defaultWindowSize :: V2 CInt
defaultWindowSize = 40 * V2 9 16

-- The main widget is limited by the screen size
render :: Widget MaxBounded MaxBounded
render = alignRight $ row $ map (limitSizeX 100 . flexibleSquare) [pink, blue, red]
-- render = centerX $ row $ [limitSizeX 100 $ flexibleSquare pink]
-- render = center $ limitSize 100 200 $ flexibleSquare blue
-- render = centerY $ flip nextTo (limitSizeY 100 $ flexibleSquare pink) $ limitSizeX 150 $ nextTo (limitSize 100 100 (flexibleSquare blue)) (limitSizeY 100 $ flexibleSquare red)
{-
render = coloredBackgroud white $ centerY $ limitSizeY 100 $ distributedX
    [ (1, flexibleSquare blue)
    , (2, flexibleSquare pink)
    , (1, flexibleSquare red )
    ]
-}

-- Works for alt-f4 or the window's X button
shouldQuit :: [SDL.Event] -> Bool
shouldQuit = any $ (SDL.QuitEvent ==) . SDL.eventPayload

mainLoop :: SDL.Window -> SDL.Renderer -> IO ()
mainLoop window renderer = do
    let loop = mainLoop window renderer

    SDL.rendererDrawColor renderer $= black
    SDL.clear renderer

    events <- SDL.pollEvents
    (V2 screenWidth screenHeight) <- get $ SDL.windowSize window

    draw renderer $ render screenWidth screenHeight
    SDL.present renderer

    -- Quit if necessary
    if shouldQuit events
      then pure ()
      else loop

main :: IO ()
main = do
    SDL.initializeAll

    window <- SDL.createWindow "App" SDL.defaultWindow { SDL.windowResizable = True, SDL.windowInitialSize = defaultWindowSize }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }

    mainLoop window renderer

    SDL.quit
