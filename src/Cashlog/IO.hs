module IO where

import Control.Exception
import Database.HDBC.Sqlite3
import System.Console.Haskeline
import System.Console.Haskeline.Completion

import Data.Types
import Data.Completion
import CompletionHelper

readString :: String
           -> Maybe String
           -> IO String
readString msg def = runInputT settings loop
  where settings = Settings noCompletion Nothing False
        loop     = do raw <- case def of
                               Just d  -> getInputLineWithInitial (msg ++ ": ") (d, "")
                               Nothing -> getInputLine (msg ++ ": ")
                      case raw of
                        Just r  -> return r
                        Nothing -> loop

readValue :: (Read a, Show a)
          => String
          -> Maybe a
          -> IO a
readValue msg def = do
    raw <- readString msg $ fmap show def
    tryRead <- try $ readIO raw :: Read a => IO ( Either SomeException a )
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
  where loop = do raw <- case def of
                           Just d  -> getInputLineWithInitial (msg ++ ": ") (show d, "")
                           Nothing -> getInputLine (msg ++ ": ")
                  case raw of
                    Nothing -> loop
                    Just r  -> return r

readArticleKey :: Connection
               -> String
               -> Maybe Int
               -> IO Int
readArticleKey con msg def = readKey msg def (completeArticle con) (mapArticleCompletionToKey con) (isArticleKey con)

readCategoryKey :: Connection
                -> String
                -> Maybe Int
                -> IO Int
readCategoryKey con msg def = readKey msg def (completeCategory con) (mapCategoryCompletionToKey con) (isCategoryKey con)

readShopKey :: Connection
            -> String
            -> Maybe Int
            -> IO Int
readShopKey con msg def = readKey msg def (completeShop con) (mapShopCompletionToKey con) (isShopKey con)

readArticle :: Connection
            -> Maybe Article
            -> IO (Either Article ArticleSkeleton) 
readArticle con art = do
    n <- readString "Bezeichnung"        $ fmap articleName art
    p <- readValue  "Preis"              $ fmap articlePrice art
    c <- readCategoryKey con "Kategorie" $ fmap articleCategoryId art
    case art of
      Just a  -> return $ Left $ Article (articleId a) n p c
      Nothing -> return $ Right (n, p, c)

readCategory :: Maybe Category
             -> IO (Either Category CategorySkeleton)
readCategory cat = do
    p <- readValue  "Oberkategorie" $ fmap categoryParent cat
    n <- readString "Name"          $ fmap categoryName cat
    case cat of
      Just c  -> return $ Left $ Category (categoryId c) p n
      Nothing -> return $ Right (p, n)

readShop :: Maybe Shop ->
            IO (Either Shop ShopSkeleton)
readShop shop = do
    n <- readString "Name"  $ fmap shopName shop
    c <- readString "Stadt" $ fmap shopCity shop
    case shop of
      Just s  -> return $ Left $ Shop (shopId s) n c
      Nothing -> return $ Right (n, c)
