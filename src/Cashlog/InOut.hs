module InOut where

import Control.Exception
import Data.Maybe
import Types

-- | The 'readValue' function promts the user for a value of
-- type 'a' until a successfully read was performed.

readValue :: Read a =>
             String ->
             Maybe a ->
             IO a
readValue msg def = do
    putStr $ msg ++ ": "
    raw <- getLine
    if isJust def && length raw == 0
        then return $ fromJust def
        else do tryRead <- try $ readIO raw :: Read a => IO ( Either SomeException a )
                case tryRead of
                  Left _ -> readValue msg def
                  Right val -> return val
                
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

