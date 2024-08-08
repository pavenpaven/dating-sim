{-# LANGUAGE TypeSynonymInstances #-} -- the string dude wtf i cant newtype that like i can mess up all other code for some stupid thing ok
{-# LANGUAGE FlexibleInstances #-}

module Main where

import System.Console.Readline
import Parse
import Safe

import Text.Read
import Text.ParserCombinators.Parsec hiding (getInput)


import Data.Maybe
import Data.List
import Data.List.Extra hiding (replace)
--import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Aeson
import Data.Aeson.Encode.Pretty
import Data.Map hiding (map, filter, take, drop, foldl)

main :: IO ()
main =
  do game <- loadGame "dialog"
     putStrLn "Welcome to scene editor type 'h' och 'help' followed by return for help"
     runEdit $ fromMaybe (error "couldnt load dialog file") game
     return ()

runEdit :: Game -> IO Game
runEdit game =
  do cmd <- getInput "> "
     addHistory cmd

     if cmd == "q" then return game
     else do game' <- runCommand cmd game
             runEdit game'

getInput :: String -> IO String
getInput prompt = readline prompt >>= (fromMaybe (getInput prompt) . fmap return)

starts = flip isPrefixOf

search :: String -> Game -> Maybe Scene
search s g = find ((s==) . name) (scenes g)

args :: String -> [String]
args = tail . words

arg :: String -> Maybe String -- gets the first object safe
arg = listToMaybe . args
             
runCommand :: String -> Game -> IO Game
runCommand s game
 | s == "list"         = (putStrLn $ show $ map name $ scenes game) >> return game
 | s == "contexts"     = (putStrLn $ intercalate "\n" $ map show $ assocs $ boolianContext $ context game) >> return game
 | s == "edit booleanContext" = (putStrLn "Change boolean Context format (String, Bool)" >> (editList $ assocs $ boolianContext $ context game))
                                >>= ((\x -> return game {context = (context game) {boolianContext = x}}) . fromList)
 | s == "edit numericContext" = (putStrLn "Change boolean Context format (String, Bool)" >> (editList $ assocs $ numericContext $ context game))
                                >>= ((\x -> return game {context = (context game) {numericContext = x}}) . fromList)                                
 | s == "create"       = createScene game
 | s `starts` "load "  = sceneEdit game $ fromMaybe "no argument given" $ arg s
 | s `starts` "print " = (putStrLn $ fromMaybe "something went wrong" $ fmap printScene $ (flip search game =<< arg s)) >> return game
 | s == "save"         = save game >> return game
 | (s == "h") || (s == "help") = putStrLn helpString >> return game
 | otherwise           = putStrLn "Command not found" >> return game
 where helpString = "type list for listing scenes type contexts for getting the context variables type create to create a new scene type load to load and be able to edit a scene type print for printing a scene type save to save changes"

printScene :: Scene -> String
printScene sce = "Name: " ++ name sce ++ "\n"
              ++ "Dialog: \n" ++ (show $ dialog sce) ++ "\n"
              ++ "Characters: \n" ++ (show $ characters sce) ++ "\n"
              ++ "Background: " ++ (show $ background sce) ++ "\n"
              ++ "link: " ++ link sce ++ "\n"
              ++ conditionals
              ++ condCommand
              ++ "dialog options: " ++ (show $ dialogOptions sce)
  where conditionals = case conditions sce of
                         Just a  -> "Conditional links: \n" ++ (intercalate "\n" $ map show a) ++ "\n"
                         Nothing -> ""

        condCommand = case commands sce of
                        Just a  ->"Conditional Commands: \n" ++ (intercalate "\n" $ map show a) ++ "\n"
                        Nothing -> ""



sceneEdit :: Game -> String -> IO Game
sceneEdit g s = fromMaybe (putStrLn "bad search" >> return g) $ fmap (success g) $ search s g 
  where success :: Game -> Scene -> IO Game
        success g s =
          do putStrLn "type h for help"
             sce <- loop s
             return $ g {scenes = sce : filter ((name s /=) . name) (scenes g)}

        loop :: Scene -> IO Scene
        loop sce =
          do cmd <- getInput (name sce ++ "> ")
             addHistory cmd

             case cmd of
               "q" -> return sce
               _   -> sceneCmd cmd sce >>= loop

        sceneCmd :: String -> Scene -> IO Scene
        sceneCmd s sce
          | s == "h" = putStrLn ("type q to exit and edit dialog | characters | background "
                             ++  "| link | conditionalLinks | conditionalCommands | dialogOptions") >> return sce
          | s == "print" || s == "p" = (putStrLn $ printScene sce) >> return sce
          | s == "edit dialog"     = putStrLn "Change dialog"                  >> (editList (dialog sce) >>= (\x -> return $ sce {dialog = x}))
          | s == "edit characters" = putStrLn "Change Characters"              >> (editList (characters sce) >>= (\x -> return $ sce {characters = x}))
          | s == "edit background" = putStrLn "Change Background (fileName)"   >> getFromInput >>= (\x -> return $ sce {background = x})
          | s == "edit link"       = putStrLn "Change Link (primary)"          >> getFromInput >>= (\x -> return $ sce {link = x})
          | s == "edit conditionalLinks" = putStrLn "Change Conditional Links" >> editMaybeList (conditions sce) >>= (\x -> return $ sce {conditions = x})
          | s == "edit conditionalCommands" = putStrLn "Change conditional commands" >> editMaybeList (commands sce) >>= (\x -> return $ sce {commands = x})
          | s == "edit dialogOptions" = putStrLn "Change dialog options"       >> editMaybeList (dialogOptions sce) >>= (\x -> return $ sce {dialogOptions = x})
          | otherwise = putStrLn "scene edit command not found type h for help and q to exit" >> return sce

replace :: Int -> a -> [a] -> [a]
replace i x l = take i l ++ [x] ++ drop (i + 1) l

removeSafe :: Int -> [a] -> Maybe [a] 
removeSafe i l = boolToMaybe (0 <= i && i < length l) $ take i l ++ drop (i + 1) l

insertInList :: Int -> a -> [a] -> [a]
insertInList i x l = take i l ++ [x] ++ drop i l

editMaybeList :: (CmdGet a, Show a) => Maybe [a] -> IO (Maybe [a])
editMaybeList m = case m of
                    Just a  -> (do answer <- getInput "Should we remove the field y/n: "; if answer == "y" then return Nothing else fmap Just $ editList a)
                    Nothing -> (do answer <- getInput "Should we add the field y/n: "; if answer == "n" then return Nothing else fmap Just $ editList [])

editList :: (CmdGet a, Show a) => [a] -> IO [a]
editList l =
  do cmd <- getInput ">> "

     if cmd == "q" then return l
       else editList =<< editCmd l cmd
     
  where
    editCmd :: (CmdGet a, Show a) => [a] -> String -> IO [a]
    editCmd l s
     | s `starts` "c " =  fromMaybe (listErr l)
                          (arg s
                       >>= fmap (+ (-1)) . (readMaybe :: String -> Maybe Int)
                       >>= (\x -> boolToMaybe (0 <= x && x < length l) x)
                       >>= (\x -> return (getFromInput >>= (\y -> return $ replace x y l))))
     | s == "p" = (putStrLn $ intercalate "\n" $ map (\(x,y) -> show x ++ ":  " ++ show y) $ zip [1..] l) >> return l
     | s `starts` "r " = fromMaybe (listErr l) ((arg s >>= fmap (+ (-1)) . readMaybe >>= fmap return . flip removeSafe l))
     | s `starts` "i " = fromMaybe (listErr l) (arg s
                                            >>= fmap (+ (-1)) . (readMaybe :: String -> Maybe Int)
                                            >>= (\x -> return (getFromInput >>= (\y -> return $ insertInList x y l))))
     | s == "a" = fmap (snoc l) $ getFromInput 
     | otherwise = putStrLn "invalid list cmd. To exit press q" >> editList l

    listErr :: a -> IO a 
    listErr l = putStrLn "something went wrong" >> return l

class CmdGet a where -- wtf but i wanted code gen so maybe this isnt super stupid o wait it is
  getFromInput :: IO a

instance CmdGet Bool where
  getFromInput =
    do a <- getInput "Enter Bool: "
       fromMaybe (putStrLn "not Bool enter True or False" >> getFromInput) $ (\x -> (case x of
                                                                                      "True"  -> Just $ return True
                                                                                      "False" -> Just $ return False
                                                                                      _       -> Nothing)) a
instance CmdGet Float where
  getFromInput =
    do a <- getInput "Enter Float: "
       fromMaybe (putStrLn "error not Float" >> getFromInput) $ fmap return $ readMaybe a


instance (CmdGet a, CmdGet b) => CmdGet (a, b) where
  getFromInput = getTuple getFromInput getFromInput

instance {-# OVERLAPPING #-} CmdGet String where -- this is like yikes maybe but i wont have to get strings by individual charaters
  getFromInput = getInput "Enter String: "

instance CmdGet a => CmdGet [a] where
  getFromInput = getList

instance CmdGet a => CmdGet (Maybe a) where
  getFromInput =
    do putStrLn "Do you want to add y/n"
       c <- readline ""
       case c of
         Just "y" -> getFromInput >>= return . Just
         Just "n" -> return Nothing
         _        -> putStrLn "Plese enter y or n" >> getFromInput



instance CmdGet Int where
  getFromInput =
    do s <- getInput "Enter Int: "
       case readMaybe s of
         Just a  -> return a
         Nothing -> putStrLn "not an int" >> getFromInput

instance CmdGet Sentence where
  getFromInput =
    do s <- getInput "Enter Sentence: "
       case parse sentence "(unkown)" s of
         Left  a -> (putStrLn $ show a) >> getFromInput
         Right b -> return b

instance CmdGet Command where
  getFromInput =
    do s <- getInput "Enter Command type: (unset or set)"
       case s of
         "set"   -> getFromInput >>= return . Set 
         "unset" -> getFromInput  >>= return . Unset
         _       -> getFromInput

instance CmdGet DialogOption where
   getFromInput =
     do putStrLn "Dialog Option line"
        l <- getFromInput
        putStrLn "List of command if this option is chosen"
        c <- getFromInput
        putStrLn "potenstial place to jump if this line is chosen"
        d <- getFromInput
        return  $ DialogOption l c d
       
data Operator = AND | OR
              deriving Show

fromOperator :: Operator -> Sentence -> Sentence -> Sentence
fromOperator o s s' = case o of
                        AND -> And s s'
                        OR  -> Or  s s'

string' s = try $ string s

sentence :: GenParser Char st Sentence
sentence =
  do char '('
     a <- clause
     char ')'
     return a

clause :: GenParser Char st Sentence
clause =
      (do left <- sentence
          char ' '
          operator <- operator
          char ' '
          right <- sentence
          return $ fromOperator operator left right)
  <|> (do string' "not "
          s <- sentence
          return (Not s))
  <|> (fmap Eval flag)

operator = (string' "and" >> return AND) <|> (string' "or" >> return OR)

flag :: GenParser Char st Flag
flag = do s <- many $ noneOf "() "
          (string' " > " >> (many (noneOf "() ") >>= (return . BiggerThan s))) <|> (return $ Flag s)


  
getItem :: String -> (String -> Maybe a) -> IO a
getItem prompt f =
  do item <- fmap (fromMaybe "") $ readline prompt
     case f item of
       Nothing -> putStrLn "bad input" >> getItem prompt f
       Just a  -> return a

getList :: CmdGet a => IO [a]
getList =
  do answer <- readline "Do you want to add a new item y/n: "
     case answer of
       Just "y" -> getFromInput >>= (\x -> getList >>=  (\y -> return (x : y)))
       Just "n" -> return []
       _        -> putStrLn "please anwser with y or n" >> getList


getTuple :: IO a -> IO b -> IO (a, b)
getTuple f s = f >>= (\x -> s >>= (\y -> return (x, y)))


createScene :: Game -> IO Game
createScene g =
  do putStrLn "Name for scene"
     name <- getFromInput

     if isJust $ search name g then return g
     else (do putStrLn "Dialog on the form (character, line)"
              dialog <- getFromInput
              putStrLn "Characters on the form (character (filename), (x position, y position))"
              characters <- getFromInput
              putStrLn "Background (filename)"
              background <- getFromInput
              putStrLn "Link to other scene"
              link <- getFromInput
              putStrLn "conditions on the form (Sentence, Link)"
              conditions <- getFromInput
              putStrLn "commands on the form (Sentence, command)" 
              commands <- getFromInput
              putStrLn "dialog options" 
              dialogOptions <- getFromInput
              return $ g {scenes = Scene name dialog characters background link conditions commands dialogOptions : scenes g})
     
             

save :: Game -> IO ()
save game = L.writeFile "dialog" $ encodePretty stored
  where stored = StoredGame (assocs $ boolianContext con, assocs $ numericContext con) (scenes game)
        con = context game

boolToMaybe :: Bool -> a -> Maybe a
boolToMaybe True a  = Just a         
boolToMaybe False a = Nothing

