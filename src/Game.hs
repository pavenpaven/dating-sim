{-# LANGUAGE BlockArguments #-}

module Game where


import qualified Data.Text as T
import Data.Vector
import Data.Monoid
import SDL
import SDL.Font hiding (Color)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Arrow
import Foreign.C.Types
import Data.Word
import Data.Int

import System.IO.Unsafe


windowConfig :: WindowConfig
windowConfig = defaultWindow {
                              windowPosition = Centered
                             ,windowGraphicsContext = OpenGLContext defaultOpenGL}


type Color = V4 Word8

type RenderingContext = ReaderT Renderer IO

newtype GraphicalProcess = MkGraphicalProcess (Kleisli IO Renderer Renderer)

type Rect = (V2 Int, V2 Int)

type Pnt = V2 Int

instance Semigroup GraphicalProcess where
  (MkGraphicalProcess a) <> (MkGraphicalProcess b) = MkGraphicalProcess (a >>> b)
  
instance Monoid GraphicalProcess where
  mempty = MkGraphicalProcess $ Kleisli (\r -> return r)

convertToV2CInt :: V2 Int -> V2 CInt
convertToV2CInt (V2 x y) = V2 (fromIntegral x) (fromIntegral y)


stdRendering :: (Renderer -> IO a) -> Color -> GraphicalProcess
stdRendering a color = MkGraphicalProcess (Kleisli (\r -> (rendererDrawColor r $= color
                                                       >> a r  
                                                       >> return r)))

textSize :: Font -> String -> (Int, Int)
textSize f s = unsafePerformIO $ SDL.Font.size f (T.pack s) -- Sinice this function should like be pure evan though it uses IO I think its worf making use of unsafePerformIO

text :: Font -> Color -> V2 Int -> String -> GraphicalProcess
text font color pos s = MkGraphicalProcess $ Kleisli
  (\r -> do{ sur <- solid font color (T.pack (if Prelude.null s then " " else s))
          ; tex <- createTextureFromSurface r sur
          ; dim <- surfaceDimensions sur
          ; SDL.copy r tex Nothing (Just $ Rectangle (P $ convertToV2CInt pos) dim)
          ; freeSurface sur
          ; destroyTexture tex
          ; return r})
     

  
rect :: Color -> Rect -> GraphicalProcess
rect color (pos, size) = stdRendering (\r -> fillRect r (Just (Rectangle (P $ convertToV2CInt pos) (convertToV2CInt size)))) color

outline :: Color -> Rect -> GraphicalProcess
outline color (pos, size) = stdRendering (\r -> drawRect r (Just (Rectangle (P $ convertToV2CInt pos) (convertToV2CInt size)))) color

line :: Color -> Pnt -> Pnt -> GraphicalProcess
line color p1 p2 = stdRendering (\r -> drawLine r (P $ convertToV2CInt p1) (P $ convertToV2CInt p2)) color

textureDimensions :: Texture -> V2 Int
textureDimensions tex = unsafePerformIO (fmap (\x -> V2 (fromIntegral $ textureWidth x) (fromIntegral $ textureHeight x)) $ queryTexture tex)

point :: Color -> Pnt -> GraphicalProcess
point color p = stdRendering (\r -> drawPoint r (P $ convertToV2CInt p)) color

display :: Texture -> Rect -> GraphicalProcess
display t (pos, size) = MkGraphicalProcess $ Kleisli (\r -> SDL.copy r t Nothing (Just $ Rectangle (P $ convertToV2CInt pos) (convertToV2CInt size)) >> return r)

defaultRendering :: (Renderer -> IO a) -> RenderingContext a
defaultRendering f = ask >>= liftIO . f

load :: FilePath -> RenderingContext Texture
load p = defaultRendering (\r -> loadBMP p >>= createTextureFromSurface r)

data GameConfig = GameConfig { fps :: Int
                             , windowConf :: WindowConfig
                             , renderConf :: RendererConfig
                             , defaultBkg :: V4 Word8}

defaultConfig :: GameConfig
defaultConfig = GameConfig { fps = 60
                           , windowConf = windowConfig
                           , renderConf = defaultRenderer
                           , defaultBkg = V4 0 0 0 255}


runGame :: GameConfig -> (RenderingContext gamestate) -> (gamestate -> GraphicalProcess) -> (gamestate -> Float -> gamestate) -> (Event -> gamestate -> gamestate) -> IO ()
runGame config game draw update eventHandle =
  do initializeAll
     SDL.Font.initialize
     win <- createWindow (T.pack "test") $ windowConf config
     render <- createRenderer win (-1) $ renderConf config

     g <- runReaderT game render
     gameLoop config render g draw update eventHandle

{-
I spent a while thinking about the text and like oh no I have to createTextureFromSurface and 
render the text every frame so I would either have to add a rendering field as an argument
and scedual rendering updates or make some kind of cache for the but then I just realized that
who cares its probobly fine.
-}

                                     
gameLoop :: GameConfig -> Renderer -> gamestate -> (gamestate -> GraphicalProcess) -> (gamestate -> Float -> gamestate) -> (Event -> gamestate -> gamestate) -> IO ()
gameLoop gameConf renderer game draw update eventHandle =
  do time1 <- ticks

     events <- pollEvents
     
     let game' = update game 1
     let game'' = foldl' (flip eventHandle) game' (Data.Vector.fromList events)

     
     let exit = Prelude.any isExitEvent events
     
     rendererDrawColor renderer $= defaultBkg gameConf
     clear renderer
   
     (runProcess $ draw game'') renderer
     present renderer

     time2 <- ticks

     let delta = time2 - time1
     let mspf = fromIntegral (1000 `div` (fps gameConf))

     delay (mspf - min delta mspf)

     unless exit $ gameLoop gameConf renderer game'' draw update eventHandle
  where
    isExitEvent :: Event -> Bool
    isExitEvent e = case eventPayload e of
                     WindowClosedEvent a -> True
                     _                   -> False
     

runProcess :: GraphicalProcess -> Renderer -> IO ()
runProcess (MkGraphicalProcess g) r = runKleisli g r >> return ()
