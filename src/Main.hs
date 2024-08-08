module Main where


import SDL
import qualified SDL.Font as F

import Control.Arrow
import Control.Monad 

import qualified Parse (scenes)
import Parse hiding (scenes)
import Game hiding (line)

import Data.List hiding (insert)
import Data.List.Extra hiding (insert)
import Data.Monoid
import Data.Maybe
import Data.Map (insert, Map, (!))
import qualified Data.Map as M

import GHC.Float

import Debug.Trace



-- things to fix: 
--   better graphics api arrows are clearly bad maybe free things
--   removing SDL import might be interresting but not really necessary i mean if its just a line who cares
--   maybe fix font things to
--   better event handling
--   - nicer api
--   - pressed keys
--   fix delta time
--   images maybe initialization

screenWidth   = 1000
screenHeight  = 800

update :: GameState -> Float -> GameState
update game _ = game

characterDimensions = V2 400 700

scaleTo = 700

scaleToHeight :: Int -> V2 Int -> V2 Int
scaleToHeight i (V2 a b) = (V2 (round (a' * (i' / b'))) (round i'))
  where i' = int2Float i
        b' = int2Float b
        a' = int2Float a

draw :: GameState -> GraphicalProcess
draw game = display (textureMap game ! background sce) (V2 0 0, V2 screenWidth screenHeight)
         <> mconcat (map (\(cha, (x,y)) -> display (textureMap game ! cha) (V2 x y, scaleToHeight scaleTo $ textureDimensions (textureMap game ! cha))) $ characters sce)
         <> infoBox
  where sce = currentScene game
        currentLine = head $ dialog sce --dont worry maybe
        infoBox = if inDialogOption game then dialogBox (dialogFont game) (fromJust $ dialogOptions $ currentScene game)
                                         else textBox (dialogFont game) (fst currentLine) (snd currentLine) 

textBoxPos          = (V2 ((screenWidth - textBoxWidth) `div` 2) (screenHeight - textBoxHeight - 20))
textBoxWidth        = 700 :: Int 
textBoxHeight       = 200 :: Int
textBoxOutlineWidth = 10  :: Int
textBoxInnerColor   = V4 0 0 200 100
textBoxOutlineColor = V4 0 0 100 100
textColor           = V4 255 255 255 255
textPadding         = 5   :: Int
newlineDistance     = 25  :: Int


box :: GraphicalProcess
box = rect textBoxOutlineColor (textBoxPos,                 V2 textBoxWidth textBoxHeight)
   <> rect textBoxInnerColor   (textBoxPos + paddingVector, V2 textBoxWidth textBoxHeight - (paddingVector * 2))

paddingVector = V2 (textBoxOutlineWidth `div` 2) (textBoxOutlineWidth `div` 2)
textVector = V2 textPadding textPadding
        
dialogBox :: F.Font -> [DialogOption] -> GraphicalProcess
dialogBox font options = textBox font "" (intercalate "\\" $ map (\(x,y) -> show x ++ ":  " ++ y ) $ zip [1..] $ map line options)

textBox :: F.Font -> String -> String -> GraphicalProcess
textBox font character dialog =
    box
 <> (mconcat $ map (\(x, y) -> text font textColor (textBoxPos + (V2 0 (x * newlineDistance)) + paddingVector + textVector) (y))
             $ zip [0..] $ (filter (not . null) (textLineSplit font =<< split (=='\\') dialog)))
 <> text font textColor (textBoxPos + V2 40 (-15)) character


textLineSplit :: F.Font -> String -> [String]
textLineSplit font s = loop (words s) []
  where loop :: [String] -> [String] -> [String]
        loop [] s     = s
        loop (x:xs) [] = loop xs [x]       
        loop (x:xs) s = if (fst (textSize font (last s ++ x)))
                         < (textBoxWidth - 2 * (textBoxOutlineWidth + textPadding)) then loop xs (init s `snoc` (last s ++ " " ++  x))
                                                                                    else loop xs (s `snoc` x)


inDialogOption :: GameState -> Bool
inDialogOption game = null (dialog $ currentScene game) && isJust (dialogOptions $ currentScene game)

isJumpCondition :: GameState -> Bool
isJumpCondition game = not (inDialogOption game) && (null $ dialog $ currentScene game)




eventHandler :: [Scene] -> Event -> GameState -> GameState
eventHandler sce event game = case eventPayload event of
                                KeyboardEvent a -> case keyboardEventKeyMotion a of
                                                    Released -> game
                                                    Pressed  -> case keysymScancode $ keyboardEventKeysym a of
                                                                 ScancodeReturn -> onReturn game
                                                                 Scancode1      -> keyNumber 1 game     
                                                                 Scancode2      -> keyNumber 2 game     
                                                                 Scancode3      -> keyNumber 3 game     
                                                                 Scancode4      -> keyNumber 4 game      
                                                                 _              -> game 
                                _               -> game
  where onReturn game = if inDialogOption game then game
                          else let game' = game {currentScene = (currentScene game) {dialog = tail $ dialog $ currentScene game}}
                                   in if isJumpCondition game'
                                      then endScene sce game'
                                      else game'

        keyNumber i game = case (dialogOptions $ currentScene game) of
                             Nothing -> game
                             Just a  -> if i <= length a then dialogOpEndScene sce game i a
                                                        else game

                                            

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates xs = loop xs []
  where loop [] s     = s
        loop (x:xs) s = if x `elem` s then loop xs s
                                      else loop xs (x:s)
            
getFileNames :: Game -> [FilePath]
getFileNames g = removeDuplicates $ ((map fst $ join $ map characters (Parse.scenes g)) ++ (map background (Parse.scenes g)))

makeTextureMap :: Game -> RenderingContext (Map String Texture)
makeTextureMap = fmap M.fromList . (mapM (\y -> load y >>= (\x -> return (y, x))) . getFileNames)


config = defaultConfig {windowConf = defaultWindow {windowInitialSize = convertToV2CInt (V2 screenWidth screenHeight)}}

main :: IO ()
main = do
  game <- loadGame "dialog" >>= return . fromJust
  let gamestate = initGameState game
  
  runGame config gamestate draw update (eventHandler $ Parse.scenes game)
  
  
search :: String -> Game -> Maybe Scene
search s g = find ((s==) . name) (Parse.scenes g)

{-
run :: IO ()
run = do
  game <- loadGame "dialog" >>= return . fromJust
  let gamestate = initGameState (context game) game
  let scenes = Parse.scenes game
  runGame scenes gamestate
-}  
  
data GameState = GameState {currentScene :: Scene, gameContext :: Context, textureMap :: Map String Texture, dialogFont :: F.Font}

fontFilePath  = "Art/animeace.ttf"
fontPointSize = 20

initGameState :: Game -> RenderingContext GameState
initGameState game =
  do texturemap <- makeTextureMap game
     f <- F.load fontFilePath fontPointSize
     return $ GameState (fromJust $ search "start" game) (context game) texturemap f



endScene :: [Scene] -> GameState -> GameState
endScene scenes gamestate = jumpToScene scenes gamestate (link $ currentScene gamestate)

dialogOpEndScene :: [Scene] -> GameState -> Int -> [DialogOption] -> GameState
dialogOpEndScene scenes gamestate n a =
  let dialogOption = a !! (n - 1) 
      context' = foldr execCommand (gameContext gamestate) (consequence dialogOption)
        in jumpToScene scenes gamestate (fromMaybe (link $ currentScene gamestate) (dialogLink dialogOption))


jumpToScene :: [Scene] -> GameState -> Name -> GameState
jumpToScene scenes gamestate l = changeScene scenes gamestate' (conditionalLink gamestate' l)
  where gamestate' = conditionalCommands gamestate

        conditionalCommands :: GameState -> GameState
        conditionalCommands g = g {gameContext = foldr checkCommand (gameContext g) (fromMaybe [] (commands $ currentScene g))}

        checkCommand :: (Sentence, Command) -> Context -> Context -- wtf is this name
        checkCommand (s, cmd) c = if eval c s then execCommand cmd c
                                              else c
        
        conditionalLink :: GameState -> Name -> Name
        conditionalLink g l = case conditions $ currentScene g of
          Just c  -> fromMaybe l $ fmap snd $ listToMaybe (filter (eval (gameContext g) . fst) c)
          Nothing -> l
  
        changeScene :: [Scene] -> GameState -> Name -> GameState
        changeScene scenes g l = g {currentScene = head $ filter ((l==) . name) scenes} -- should use find
       


execCommand :: Command -> Context -> Context
execCommand cmd c =
  case cmd of
    Set   f -> c {boolianContext = insert f True  $ boolianContext c}
    Unset f -> c {boolianContext = insert f False $ boolianContext c}

