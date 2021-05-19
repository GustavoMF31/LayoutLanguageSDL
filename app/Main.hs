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
-- render = flexibleSquare pink
render = alignLeft $ row
    [ fixedSize 100 $ flexibleSquare pink
    , fixedSize 100 $ flexibleSquare blue
    , fixedSize 100 $ flexibleSquare red
    -- , fixedSizeX 100 $ flexibleSquare blue
    -- , fixedSizeX 100 $ flexibleSquare red
    ]

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
