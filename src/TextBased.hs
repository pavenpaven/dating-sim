module Main where

import Parse hiding (scenes)
import qualified Parse (scenes)
import Control.Monad 
import Data.Maybe
import Data.Map (insert, Map)


main :: IO ()
main = do
  game <- loadGame "dialog" >>= return . fromJust
  let gamestate = initGameState (context game)
  let scenes = Parse.scenes game
  runGame scenes gamestate  
  
data GameState = GameState {currentScene :: Scene, gameContext :: Context}

initGameState context = GameState (Scene "" [] [] "" "start" Nothing Nothing Nothing) context


runGame :: [Scene] -> GameState -> IO ()
runGame scenes gamestate =
  case currentDialog of
    []   -> endScene scenes gamestate >>= runGame scenes
    x:xs -> (do
             putStrLn $ fst x ++ ":"
             putStrLn $ snd x
             getLine
             runGame scenes (gamestate {currentScene = (currentScene gamestate) {dialog = xs}}))      
  where currentDialog = dialog $ currentScene gamestate

endScene :: [Scene] -> GameState -> IO GameState
endScene scenes gamestate =
  case dialogOptions $ currentScene gamestate of
    Just a  ->
      do putStrLn $ show $ map line a
         putStrLn "enter 1,2 ... "
         n <- getLine >>= return . read :: IO Int
         if (n > length a) then endScene scenes gamestate
         else (do let dialogOption = a !! (n - 1)
                  let context' = foldr execCommand (gameContext gamestate) (consequence dialogOption)
                  return $ jumpToScene scenes gamestate (fromMaybe (link $ currentScene gamestate) (dialogLink dialogOption)))
    Nothing -> return $ jumpToScene scenes gamestate (link $ currentScene gamestate)

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


