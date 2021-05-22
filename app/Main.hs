{-# LANGUAGE OverloadedStrings, DataKinds #-}

import Foreign.C.Types (CInt)

import qualified SDL
import SDL (V2(..), get, ($=))
import SDL.Font (Font)
import qualified SDL.Font as Font

import Widget --(Widget, WidgetSizeDependency(..), flexibleSquare, row, limit, alignLeft, alignTop)
import Drawable

defaultWindowSize :: V2 CInt
defaultWindowSize = 40 * V2 9 16

fab :: Widget MaxBounded MaxBounded
fab = marginAround 30 $ alignBottom $ alignRight $ limitSize 70 70 $ flexibleCircle lightBlue

appBar :: Widget MaxBounded ConstantSized
appBar = limitSizeY 60 $ flexibleSquare lightBlue

-- TODO: Y-axis versions of distributedX and spaceEvenlyX
-- TODO: images, writing text, rudimentary shadows, card, polygons

-- The main widget is limited by the screen size
render :: Font -> Widget MaxBounded MaxBounded
render font = coloredBackgroud white $ alignRight $ alignBottom $ text font "Hello world!"
-- render = atop appBar (overlay (flexibleSquare white) fab)
-- render = overlay (flexibleSquare white) $
--     spaceEvenlyX $ map (center . limitSize 100 100 . flexibleCircle) [red, blue, pink]

-- render = marginAround 100 $ aspectRatio (1/1) $ flexibleCircle pink
-- render = alignRight $ row $ map (limitSizeX 100 . flexibleSquare) [pink, blue, red]
-- render = centerX $ row $ [limitSizeX 100 $ flexibleSquare pink]
-- render = center $ limitSize 100 200 $ flexibleSquare blue
-- render = centerY $ flip nextTo (limitSizeY 100 $ flexibleSquare pink) $ limitSizeX 150 $ nextTo (limitSize 100 100 (flexibleSquare blue)) (limitSizeY 100 $ flexibleSquare red)
--
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

mainLoop :: Font -> SDL.Window -> SDL.Renderer -> IO ()
mainLoop font window renderer = do
    let loop = mainLoop font window renderer

    SDL.rendererDrawColor renderer $= black
    SDL.clear renderer

    events <- SDL.pollEvents
    (V2 screenWidth screenHeight) <- get $ SDL.windowSize window

    draw renderer $ render font screenWidth screenHeight
    SDL.present renderer

    -- Quit if necessary
    if shouldQuit events
      then pure ()
      else loop

main :: IO ()
main = do
    SDL.initializeAll
    Font.initialize

    window <- SDL.createWindow "App" SDL.defaultWindow { SDL.windowResizable = True, SDL.windowInitialSize = defaultWindowSize }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }
    roboto <- Font.load "fonts/Roboto/Roboto-Regular.ttf" 24

    mainLoop roboto window renderer

    Font.quit
    SDL.quit
