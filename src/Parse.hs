{-# LANGUAGE DeriveGeneric #-}

module Parse where

import Data.Aeson
import Data.Map
import GHC.Generics

type Name = String

data Scene = Scene {name :: Name 
                   ,dialog :: [(Name, String)]
                   ,characters :: [(FilePath, (Int, Int))]
                   ,background :: FilePath                
                   ,link :: Name
                   ,conditions :: Maybe [(Sentence, Name)]
                   ,commands :: Maybe [(Sentence, Command)]
                   ,dialogOptions :: Maybe [DialogOption]}
             deriving (Show, Generic)

data DialogOption = DialogOption {line :: String
                                 ,consequence :: [Command]
                                 ,dialogLink :: Maybe String} -- to make things less bloated in json
                    deriving (Show, Generic)

instance ToJSON DialogOption --where
--  toEncoding = genericToEncoding sceneOptions

instance FromJSON DialogOption --where
--  parseJSON = genericParseJSON sceneOptions

type StrFlag = String

data Command = Set   StrFlag
             | Unset StrFlag
             deriving (Show, Generic)

instance ToJSON Command --where
--  toEncoding = genericToEncoding sentenceOptions

instance FromJSON Command --where
--  parseJSON = genericParseJSON sentenceOptions

type Character = String

sceneOptions = defaultOptions {omitNothingFields  = True
                             ,allowOmittedFields = True
                             ,allNullaryToStringTag = False
                             ,sumEncoding = ObjectWithSingleField}

type NumericValue = String

data Flag = Flag String
          | BiggerThan NumericValue NumericValue
          deriving (Show, Eq, Generic)


sentenceOptions = defaultOptions {allNullaryToStringTag = False, sumEncoding = ObjectWithSingleField}

instance ToJSON Flag --where
--  toEncoding = genericToEncoding sentenceOptions

instance FromJSON Flag --where
--  parseJSON = genericParseJSON sentenceOptions

data Sentence = And Sentence Sentence
              | Or  Sentence Sentence
              | Not Sentence
              | Eval Flag
              deriving (Show, Eq, Generic)

instance ToJSON Sentence --where
  --toEncoding = genericToEncoding sentenceOptions

instance FromJSON Sentence --where
  --parseJSON = genericParseJSON sentenceOptions


instance ToJSON Scene --where
  --toEncoding = genericToEncoding sceneOptions
  
instance FromJSON Scene --where
  --parseJSON = genericParseJSON sentenceOptions
 

data Context = Context {boolianContext :: Map String Bool
                       ,numericContext :: Map NumericValue Float}
               deriving (Show, Generic)

instance ToJSON Context
--
instance FromJSON Context
--

data Game = Game {context :: Context, scenes :: [Scene]}
          deriving Show

data StoredGame = StoredGame {s_context :: ([(String, Bool)], [(String, Float)]), s_scenes :: [Scene]}
          deriving (Show, Generic)

instance ToJSON StoredGame --where
  --toEncoding = genericToEncoding sceneOptions -- why is it not recursive like seriously maybe some anoying thing becouse like this isnt optimal

instance FromJSON StoredGame --where
  --parseJSON = genericParseJSON sentenceOptions

getGame :: StoredGame -> Game
getGame g = Game (Context (Data.Map.fromList $ fst $ s_context g)
                          (Data.Map.fromList $ snd $ s_context g))
                 (s_scenes g)

loadGame :: FilePath -> IO (Maybe Game)
loadGame file = do
  storedGame <- decodeFileStrict file :: IO (Maybe StoredGame)
  return $ fmap getGame storedGame


eval :: Context -> Sentence -> Bool
eval c (Eval flag) = case flag of
                       Flag f          -> boolianContext c ! f
                       BiggerThan s s' -> numericContext c ! s > numericContext c ! s'
eval c (And r l)   = eval c r && eval c l
eval c (Or r l)    = eval c r || eval c l
eval c (Not s)     = not $ eval c s


loadScenes :: FilePath -> IO (Maybe [Scene])
loadScenes = decodeFileStrict
