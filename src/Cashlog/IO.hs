module IO where

import Data.Maybe
import Control.Exception
import Database.HDBC.Sqlite3
import System.Console.Haskeline
import System.Console.Haskeline.Completion
import Data.Time hiding (readTime)
import System.Locale

import Data.Types
import Data.Completion
import CompletionHelper

dateFormat = "%d.%m.%Y"
timeFormat = "%R"

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

readDate :: String
         -> Maybe Day
         -> IO Day
readDate msg def = do
    d <- readString msg defDate
    case (parseTime defaultTimeLocale dateFormat d) of
      Just d' -> return d'
      Nothing -> readDate msg def
  where defDate = fmap (formatTime defaultTimeLocale dateFormat) def

readTime :: String
         -> Maybe TimeOfDay
         -> IO TimeOfDay
readTime msg def = do
    t <- readString msg defTime
    case parseTime defaultTimeLocale timeFormat t of
      Just t' -> return t'
      Nothing -> readTime msg def
  where defTime = fmap (formatTime defaultTimeLocale timeFormat) def
{-
readDateTime :: String
             -> Maybe UTCTime
             -> IO (Maybe UTCTime)
readDateTime def = do
    d <- readString "Datum" defDate
    t <- readString "Uhrzeit" defTime
    fmap show getCurrentTime
where defDate = fmap (formatTime defaultTimeLocale dateFormat) (fmap utctDay mutc) 
      defTime = fmap (formatTime defaultTimeLocale dateFormat) (fmap utctDay mutc)
-}
readKey :: String
        -> Maybe Int
        -> (String -> IO [String])
        -> (Int -> IO (Maybe String))
        -> (String -> IO (Maybe Int))
        -> (Int -> IO Bool)
        -> IO Int
readKey msg def fcomp fmapcomp fmapkey fchkkey = do
    defStr <- case def of
                Just defInt -> do defStr' <- fmapcomp defInt
                                  case defStr' of
                                    Just s  -> return s
                                    Nothing -> return $ show defInt
                Nothing     -> return ""
    inp <- runInputT (Settings (completeWord Nothing " " (simpleWordCompletion fcomp)) Nothing False) (loop defStr)
    tryReadInt <- try $ readIO inp :: IO (Either SomeException Int)
    case tryReadInt of
      Left _    -> do mkkey <- fmapkey inp
                      case mkkey of
                        Just key -> return key
                        Nothing  -> readKey msg def fcomp fmapcomp fmapkey fchkkey
      Right key -> do chkkey <- fchkkey key
                      case chkkey of
                        True -> return key
                        _    -> readKey msg def fcomp fmapcomp fmapkey fchkkey
  where loop def' = do
            raw <- getInputLineWithInitial (msg ++ ": ") (def', "")
            case raw of
              Just r  -> return r
              Nothing -> loop def'

readArticleKey :: Connection
               -> String
               -> Maybe Int
               -> IO Int
readArticleKey con msg def = readKey msg def (completeArticle con) (mapArticleKeyToCompletion con) (mapArticleCompletionToKey con) (isArticleKey con)


readCategoryKey :: Connection
                -> String
                -> Maybe Int
                -> IO Int
readCategoryKey con msg def = readKey msg def (completeCategory con) (mapCategoryKeyToCompletion con) (mapCategoryCompletionToKey con) (isCategoryKey con)

readShopKey :: Connection
            -> String
            -> Maybe Int
            -> IO Int
readShopKey con msg def = readKey msg def (completeShop con) (mapShopKeyToCompletion con) (mapShopCompletionToKey con) (isShopKey con)

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

