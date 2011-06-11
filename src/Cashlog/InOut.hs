module InOut where

import Control.Exception
import Data.Maybe
import Types

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

-- | The 'readString' function is a specialization of the 'readValue' function.
readString :: String -> Maybe String -> IO String
readString msg def = do
    putStr $ msg ++ ": "
    str <- getLine
    return $ case (str,def) of
      ([], Just d) -> d
      _            -> str

readArticle :: Maybe Article ->
               IO Article
readArticle a = do
    n <- readString "Bezeichnung" $ fmap articleName a
    p <- readValue  "Preis"       $ fmap articlePrice a
    c <- readValue " Kategorie"   $ fmap articleCategoryId a
    return $ Article  (-1) n p c

readCategory :: Maybe Category ->
                IO Category
readCategory c = do
    p <- readValue  "Oberkategorie" $ fmap categoryParent c
    n <- readString "Name"          $ fmap categoryName c
    return $ Category (-1) p n

readShop :: Maybe Shop ->
            IO Shop
readShop s = do
    n <- readString "Name"  $ fmap shopName s
    c <- readString "Stadt" $ fmap shopCity s
    return $ Shop (-1) n c

