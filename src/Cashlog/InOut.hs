module InOut where

import Control.Exception
import Data.Maybe
import Types

-- | The 'readValue'' function promts the user for a value of
-- type 'a' until a successfully read was performed.
readValue' :: Read a => 
              String ->
              IO a
readValue' msg = do
	putStr $ msg ++ ": "
	tryRead <- try readLn :: Read a => IO (Either SomeException a)
	case tryRead of
		Left _ -> readValue' msg
		Right val -> return val

-- | The 'readValue''' function is similar to 'readValue'' 
-- except that it returns a default value if input was empty.
readValue'' :: Read a => 
               String ->
               a ->
               IO a
readValue'' msg def = do
    putStr $ msg ++ ": "
    raw <- getLine
    case raw of
        [] -> return def
        _ -> do tryRead <- try $ readIO raw :: Read a => IO ( Either SomeException a )
                case tryRead of
                    Left e -> do print e 
                                 readValue'' msg def
                    Right val -> return val

-- | The 'readValue' function decides wether to use 'readValue'' or 'readValue'''
-- depending on if a default value was given or not.
readValue :: Read a =>
             String ->
             Maybe a ->
             IO a
readValue msg (Just def) = readValue'' msg def
readValue msg _ = readValue' msg

-- | The 'maybeAccessor' function applies the accessor 'f' 
-- if a valid 'a' is given.
maybeAccessor :: Read b =>
                 ( a -> b ) ->
                 Maybe a ->
                 Maybe b
maybeAccessor f (Just a) = Just $ f a
maybeAccessor _ _ = Nothing


readArticle :: Maybe Article ->
               IO Article
readArticle a = do
    n <- readValue "Bezeichnung" ( maybeAccessor articleName a )
    p <- readValue "Preis" ( maybeAccessor  articlePrice a )
    c <- readValue "Kategorie" ( maybeAccessor articleCategoryId a )
    return $ Article  (-1) n p c

