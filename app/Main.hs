{-# LANGUAGE OverloadedStrings, DataKinds #-}

import Foreign.C.Types (CInt)

import qualified SDL
import qualified SDL.Video.Renderer as Renderer
import SDL (V2(..), get, ($=))
import SDL.Font (Font)
import qualified SDL.Font as Font
import qualified SDL.Image as Image
import SDL.Framerate (Manager)
import qualified SDL.Framerate as Framerate

import Widget --(Widget, WidgetSizeDependency(..), flexibleSquare, row, limit, alignLeft, alignTop)
import Drawable

defaultWindowSize :: V2 CInt
defaultWindowSize = 40 * V2 9 16

fab :: SDL.Texture -> Widget Constant Constant
fab icon = beneath (constMarginAround 15 $ image 25 25 icon) $ flexibleCircle lightBlue

appBar :: Font -> Widget Flexible Constant
appBar font = matchHeightBelow
    (alignLeft $ constMarginAround 15 $ text font white "Flutter Demo Home Page")
    (flexibleSquare lightBlue)

flutterPage :: Font -> Font -> Font -> SDL.Texture -> Widget Flexible Flexible
flutterPage regularFont smallFont zeroFont icon = atop (appBar regularFont) $ stack $
    [ flexibleSquare white -- White background
    , centerY $ column $ map centerX -- Text content in the center
        [ text smallFont darkGray "You have pushed the button this many times:"
        , text zeroFont darkGray "0"
        ]
    , (marginAround 15 $ alignRight $ alignBottom $ fab icon) -- FAB
    ]

-- TODO: Y-axis versions of distributedX and spaceEvenlyX
-- TODO: rudimentary shadows, card, polygons

flexibleMiddleX :: Widget Constant a -> Widget Flexible a -> Widget Constant a -> Widget Flexible a
flexibleMiddleX a b c = after (a `before` b) c

flexibleMiddleY :: Widget a Constant -> Widget a Flexible -> Widget a Constant -> Widget a Flexible
flexibleMiddleY a b c = atop a (b `above` c)

squareMosaic :: Widget Flexible Flexible
squareMosaic = marginAround margin $ flexibleMiddleY
    (flexibleMiddleX (cornerSquare pink      ) (horizontalRectangle blue) (cornerSquare white    ))
    (flexibleMiddleX (verticalRectangle white) (middleSquare pink       ) (verticalRectangle blue))
    (flexibleMiddleX (cornerSquare darkGray  ) (horizontalRectangle red ) (cornerSquare lightBlue))
  where
    size = 100
    margin = 10
    middleSquare = marginAround margin . flexibleSquare
    cornerSquare = limitSize size size . marginAround margin . flexibleSquare
    horizontalRectangle = limitSizeY size . marginAround margin . flexibleSquare
    verticalRectangle = limitSizeX size . marginAround margin . flexibleSquare

-- The main widget is limited by the screen size
render :: Font -> Font -> Font -> SDL.Texture -> Widget Flexible Flexible
render _ _ _ _ = squareMosaic

{-
render = flutterPage
render font addIcon = coloredBackgroud white $ alignRight $ alignBottom $ text font "Hello world!"
render = atop appBar (overlay (flexibleSquare white) fab)
render = overlay (flexibleSquare white) $
    spaceEvenlyX $ map (center . limitSize 100 100 . flexibleCircle) [red, blue, pink]
render = marginAround 100 $ aspectRatio (1/1) $ flexibleCircle pink
render = alignRight $ row $ map (limitSizeX 100 . flexibleSquare) [pink, blue, red]
render = centerX $ row $ [limitSizeX 100 $ flexibleSquare pink]
render = center $ limitSize 100 200 $ flexibleSquare blue
render = centerY $ flip nextTo (limitSizeY 100 $ flexibleSquare pink) $ limitSizeX 150 $ nextTo (limitSize 100 100 (flexibleSquare blue)) (limitSizeY 100 $ flexibleSquare red)
render = coloredBackgroud white $ centerY $ limitSizeY 100 $ distributedX
    [ (1, flexibleSquare blue)
    , (2, flexibleSquare pink)
    , (1, flexibleSquare red )
    ]
-}

-- Works for alt-f4 or the window's X button
shouldQuit :: [SDL.Event] -> Bool
shouldQuit = any $ (SDL.QuitEvent ==) . SDL.eventPayload

mainLoop :: Font -> Font -> Font -> SDL.Texture -> SDL.Window -> SDL.Renderer -> Manager -> IO ()
mainLoop regularFont smallFont zeroFont addIcon window renderer manager = do
    let loop = Framerate.delay_ manager >> mainLoop regularFont smallFont zeroFont addIcon window renderer manager

    SDL.rendererDrawColor renderer $= black
    SDL.clear renderer

    events <- SDL.pollEvents
    (V2 screenWidth screenHeight) <- get $ SDL.windowSize window

    draw renderer $ render regularFont smallFont zeroFont addIcon screenWidth screenHeight
    SDL.present renderer

    -- Quit if necessary
    if shouldQuit events
      then pure ()
      else loop

main :: IO ()
main = do
    SDL.initializeAll
    Font.initialize
    -- Explicit initialization of SDL.Image is optional

    window <- SDL.createWindow "App" SDL.defaultWindow { SDL.windowResizable = True, SDL.windowInitialSize = defaultWindowSize }
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer { SDL.rendererType = SDL.AcceleratedVSyncRenderer }

    -- Those calls to load can crash if the file is not found
    robotoMedium <- Font.load "fonts/Roboto/Roboto-Medium.ttf" 20
    robotoSmall <- Font.load "fonts/Roboto/Roboto-Regular.ttf" 14
    robotoZero <- Font.load "fonts/Roboto/Roboto-Medium.ttf" 34
    addIcon <- Image.loadTexture renderer "images/add_white.png"

    Framerate.with 60 $ mainLoop robotoMedium robotoSmall robotoZero addIcon window renderer

    Renderer.destroyTexture addIcon
    SDL.destroyWindow window
    SDL.destroyRenderer renderer
    Font.free robotoMedium
    Font.free robotoSmall
    Font.free robotoZero

    Image.quit
    Font.quit
    SDL.quit
