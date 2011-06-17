module InOut where

import Data.List
import Data.Char
import Control.Exception
import Database.HDBC
import Database.HDBC.Sqlite3
import System.Console.Haskeline
import System.Console.Haskeline.Completion

import Types
import Database

commonPrefix :: [String]
             -> String
commonPrefix []        = []
commonPrefix (([]):_)  = []
commonPrefix xss@(x:_) = fst $ foldl prefix ([], True) (inits x)
  where isCommonPrefix p    = and $ map (isPrefixOf p) xss
        prefix (o, True) n  = case isCommonPrefix n of
                                True -> (n, True)
                                _    -> (o, False)
        prefix (o, False) _ = (o, False)

simpleWordCompletion :: (String -> IO [String])
                     -> String
                     -> IO [Completion]
simpleWordCompletion fcomp word = do
    compList <- fcomp word
    case compList of
      [] -> return []
      _  -> do let compList' = map (drop $ length word) compList
                   prefix    = commonPrefix compList'
               return $ map (makeCompletion prefix) compList
  where makeCompletion [] comp = Completion word comp False
        makeCompletion p comp  = Completion (word ++ p) comp False

-- | The 'readString' function prompts the user for input.
readString :: String -> Maybe String -> IO String
readString msg def = do
    putStr $ msg ++ ": "
    str <- getLine
    return $ case (str,def) of
      ([], Just d) -> d
      _            -> str

-- | The 'readValue' function promts the user for a value of
-- type 'a' until a successfully read was performed.
readValue :: Read a => String -> Maybe a -> IO a
readValue msg def = do
    raw <- readString msg Nothing
    case (raw,def) of
      ([], Just d) -> return d
      _            -> do tryRead <- try $ readIO raw :: Read a => IO ( Either SomeException a )
                         case tryRead of
                           Left _ -> readValue msg def
                           Right val -> return val

readKey :: String
        -> Maybe Int
        -> (String -> IO [String])
        -> (String -> IO (Maybe Int))
        -> (Int -> IO Bool)
        -> IO Int
readKey msg def fcomp fmkkey fchkkey = do
    inp <- runInputT (Settings (completeWord Nothing " " (simpleWordCompletion fcomp)) Nothing False) loop
    tryReadInt <- try $ readIO inp :: IO (Either SomeException Int)
    case tryReadInt of
      Left _    -> do mkkey <- fmkkey inp
                      case mkkey of
                        Just key -> return key
                        Nothing  -> readKey msg def fcomp fmkkey fchkkey
      Right key -> do chkkey <- fchkkey key
                      case chkkey of
                        True -> return key
                        _    -> readKey msg def fcomp fmkkey fchkkey
  where loop :: InputT IO String
        loop = do raw <- case def of
                           Just d  -> getInputLineWithInitial (msg ++ ": ") (show d, "")
                           Nothing -> getInputLine (msg ++ ": ")
                  case raw of
                    Nothing -> loop
                    Just r  -> return r

readArticleKey :: Connection
               -> String
               -> Maybe Int
               -> IO Int
readArticleKey con msg def = readKey msg def (completeArticle con) (articleIdFromName con) (isArticleKeyValid con)

readArticle :: Maybe Article ->
               IO Article
readArticle a = do
    n <- readString "Bezeichnung" $ fmap articleName a
    p <- readValue  "Preis"       $ fmap articlePrice a
    c <- readValue " Kategorie"   $ fmap articleCategoryId a
    let i = maybe (-1) articleId a
    return $ Article  i n p c

readCategory :: Maybe Category ->
                IO Category
readCategory c = do
    p <- readValue  "Oberkategorie" $ fmap categoryParent c
    n <- readString "Name"          $ fmap categoryName c
    let i = maybe (-1) categoryId c
    return $ Category i p n

readShop :: Maybe Shop ->
            IO Shop
readShop s = do
    n <- readString "Name"  $ fmap shopName s
    c <- readString "Stadt" $ fmap shopCity s
    let i = maybe (-1) shopId s
    return $ Shop i n c

