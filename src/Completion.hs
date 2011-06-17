module Completion where

import Data.List
import Data.Char
import System.Console.Haskeline
import System.Console.Haskeline.Completion

wl = [ "Hausrat"
     , "Hausbau"
     , "Hausfrau"
     , "Haudegen" ]

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

commonPrefix' :: [String]
              -> String
commonPrefix' []        = []
commonPrefix' (([]):_)  = []
commonPrefix' xss@(x:_) = fst $ foldl prefix ([], True) prefixes
  where prefixes            = map (map toLower) (inits x)
        xss'                = map (map toLower) xss
        isCommonPrefix p    = and $ map (isPrefixOf p) xss'
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

