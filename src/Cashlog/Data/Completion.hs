{-# LANGUAGE FlexibleContexts #-}
module Data.Completion where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Convertible.Base

wrapPair :: String
         -> String
         -> String
wrapPair n1 n2 = n1 ++ "(" ++ n2 ++ ")"

unwrapPair :: String
           -> (String,String)
unwrapPair p = let (n1,n2) = break (\b -> b == '(') p
               in  (n1, filter (\s -> not (s == '(' || s == ')')) n2)

wrapTriple :: String
           -> String
           -> String
           -> String
wrapTriple n1 n2 n3 = n1 ++ "(" ++ n2 ++ "/" ++ n3 ++ ")"

unwrapTriple :: String
             -> (String, String, String)
unwrapTriple t = let (n1,n23) = unwrapPair t
                     (n2,n3)  = break (\b -> b == '/') n23
                     n3'      = filter (\f -> f /= '/') n3
                 in  (n1,n2,n3')

justTopLeft :: Convertible SqlValue a
            => [[SqlValue]]
            -> Maybe a
justTopLeft r = case r of
                  (x:[]) -> Just $ fromSql $ head x
                  _      -> Nothing

isPrimaryKeyValid :: Connection
                  -> String
                  -> Int
                  -> IO Bool
isPrimaryKeyValid con table id = do
    queryResult <- quickQuery' con ("SELECT id FROM " ++ table ++ " WHERE id = ?") [toSql id]
    return $ not $ null queryResult

isArticleKey con = isPrimaryKeyValid con "article"
isCategoryKey con = isPrimaryKeyValid con "category"
isShopKey con = isPrimaryKeyValid con "shop"
isVoucherKey con = isPrimaryKeyValid con "voucher"

completeArticle :: Connection
                -> String
                -> IO [String]
completeArticle con word = do
    queryResult <- quickQuery' con ("SELECT name FROM article WHERE name LIKE '" ++ word ++"%'") []
    return $ map (\(n:[]) -> fromSql n) queryResult

mapArticleCompletionToKey :: Connection
                          -> String
                          -> IO (Maybe Int)
mapArticleCompletionToKey con comp = do
    queryResult <- quickQuery' con "SELECT id FROM article WHERE name = ?" params
    return $ justTopLeft queryResult 
  where params = [ toSql comp ]

mapArticleKeyToCompletion :: Connection
                          -> Int
                          -> IO (Maybe String)
mapArticleKeyToCompletion con comp = do
    queryResult <- quickQuery' con "SELECT id FROM article WHERE name = ?" params
    return $ justTopLeft queryResult
  where params = [ toSql comp ]

completeCategory :: Connection
                 -> String
                 -> IO [String]
completeCategory con word = do
    queryResult <- quickQuery' con ("SELECT name FROM category WHERE name LIKE '" ++ word ++ "%'") []
    return $ map (\(n:[]) -> fromSql n) queryResult

mapCategoryCompletionToKey :: Connection
                           -> String
                           -> IO (Maybe Int)
mapCategoryCompletionToKey con comp = do
    queryResult <- quickQuery' con "SELECT id FROM category WHERE name = ?" params
    return $ justTopLeft queryResult
  where params = [ toSql comp ]

mapCategoryKeyToCompletion :: Connection
                           -> Int
                           -> IO (Maybe String)
mapCategoryKeyToCompletion con key = do
    queryResult <- quickQuery' con "SELECT name FROM category WHERE id = ?" params
    return $ justTopLeft queryResult
  where params = [ toSql key ]

completeShop :: Connection
             -> String
             -> IO [(String)]
completeShop con nc = do
    let (n,c) = unwrapPair nc
    queryResult <- quickQuery' con ("SELECT name, city FROM shop WHERE name LIKE '" ++ n ++ "%' AND city LIKE '" ++ c ++ "%'") []
    return $ map (\(n:c:[]) -> wrapPair (fromSql n) (fromSql c)) queryResult

mapShopCompletionToKey :: Connection
                       -> String
                       -> IO (Maybe Int)
mapShopCompletionToKey con comp = do
    let (n,c) = unwrapPair comp
        (n',c') = (toSql n, toSql c)
    queryResult <- quickQuery' con "SELECT id FROM shop WHERE name = ? AND city = ?" [n', c']
    case queryResult of
      (x:[]) -> return $ Just $ fromSql $ head x
      _      -> return Nothing

mapShopKeyToCompletion :: Connection
                       -> Int
                       -> IO (Maybe String)
mapShopKeyToCompletion con key = do
    queryResult <- quickQuery' con "SELECT name, city FROM shop WHERE id = ?" params
    return $ justTopLeft queryResult
  where params = [ toSql key ]

