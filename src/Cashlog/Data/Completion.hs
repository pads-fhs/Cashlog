module Cashlog.Data.Completion 
    ( articleCompletion
    , categoryCompletion
    , shopCompletion
    , voucherCompletion
    , mapArticleCompletionToKey
    , mapArticleKeyToCompletion
    , mapCategoryCompletionToKey
    , mapCategoryKeyToCompletion
    , mapShopCompletionToKey
    , mapShopKeyToCompletion
    , mapVoucherCompletionToKey
    , mapVoucherKeyToCompletion
    , isArticleKey
    , isCategoryKey
    , isShopKey
    , isVoucherKey
    ) where

import qualified Database.HDBC                       as DB
import qualified System.Console.Haskeline.Completion as HKLC
import Data.List

import Cashlog.Data.Connection
import Cashlog.Data.Utility

isPrimaryKeyValid :: DataHandle
                  -> String
                  -> Int
                  -> IO Bool
isPrimaryKeyValid handle table key = do
    result <- DB.quickQuery' handle
                             ( "SELECT id \
                               \FROM " ++ table ++ " \
                               \WHERE id = ?" )
                             [DB.toSql key]
    return $ not $ null result

isArticleKey handle  = isPrimaryKeyValid handle "article"
isCategoryKey handle = isPrimaryKeyValid handle "category"
isShopKey handle     = isPrimaryKeyValid handle "shop"
isVoucherKey handle  = isPrimaryKeyValid handle "voucher"

completeArticle :: DataHandle
                -> String
                -> IO [String]
completeArticle handle word = do
    result <- DB.quickQuery' handle 
                             ( "SELECT name \
                               \FROM article \
                               \WHERE name \
                               \LIKE '" ++ word ++"%'" )
                             []
    return $ map (\(n:[]) -> DB.fromSql n) result

mapArticleCompletionToKey :: DataHandle
                          -> String
                          -> IO (Maybe Int)
mapArticleCompletionToKey handle comp = do
    result <- DB.quickQuery' handle
                             "SELECT id FROM article WHERE name = ?"
                             params
    return $ justTopLeft result 
  where params = [ DB.toSql comp ]

mapArticleKeyToCompletion :: DataHandle
                          -> Int
                          -> IO (Maybe String)
mapArticleKeyToCompletion handle comp = do
    result <- DB.quickQuery' handle
                             "SELECT id FROM article WHERE name = ?"
                             params
    return $ justTopLeft result
  where params = [ DB.toSql comp ]

completeCategory :: DataHandle
                 -> String
                 -> IO [String]
completeCategory handle word = do
    result <- DB.quickQuery' handle
                             ( "SELECT name \
                               \FROM category \
                               \WHERE name \
                               \LIKE '" ++ word ++ "%'" )
                             []
    return $ map (\(n:[]) -> DB.fromSql n) result

mapCategoryCompletionToKey :: DataHandle
                           -> String
                           -> IO (Maybe Int)
mapCategoryCompletionToKey handle comp = do
    result <- DB.quickQuery' handle
                             "SELECT id \
                             \FROM category \
                             \WHERE name = ?"
                             params
    return $ justTopLeft result
  where params = [ DB.toSql comp ]

mapCategoryKeyToCompletion :: DataHandle
                           -> Int
                           -> IO (Maybe String)
mapCategoryKeyToCompletion handle key = do
    result <- DB.quickQuery' handle
                             "SELECT name \
                             \FROM category \ 
                             \WHERE id = ?"
                             params
    return $ justTopLeft result
  where params = [ DB.toSql key ]

completeShop :: DataHandle
             -> String
             -> IO [(String)]
completeShop handle nc = do
    let (n,c) = unwrapPair nc
    result <- DB.quickQuery' handle
                             ( "SELECT name, city \
                               \FROM shop \
                               \WHERE name LIKE '" ++ n ++ "%' \
                               \AND city LIKE '" ++ c ++ "%'" )
                             []
    return $ map (\(n:c:[]) -> wrapPair (DB.fromSql n) (DB.fromSql c)) result

mapShopCompletionToKey :: DataHandle
                       -> String
                       -> IO (Maybe Int)
mapShopCompletionToKey handle comp = do
    let (n,c) = unwrapPair comp
        (n',c') = (DB.toSql n, DB.toSql c)
    result <- DB.quickQuery' handle
                             "SELECT id \
                             \FROM shop \
                             \WHERE name = ? \
                             \AND city = ?"
                             [n', c']
    return $ justTopLeft result

mapShopKeyToCompletion :: DataHandle
                       -> Int
                       -> IO (Maybe String)
mapShopKeyToCompletion handle key = do
    result <- DB.quickQuery' handle
                             "SELECT name, city \
                             \FROM shop \
                             \WHERE id = ?"
                             params
    case result of
      ((name:city:[]):[]) -> return $ Just
                                    $ wrapPair (DB.fromSql name)
                                               (DB.fromSql city)
      otherwise           -> return Nothing
  where params = [ DB.toSql key ]

completeVoucher :: DataHandle
                -> String
                -> IO [(String)]
completeVoucher handle ts = do
    let (t,n) = unwrapPair ts
    result <- DB.quickQuery' handle
                             ( "SELECT v.timestamp, s.name \
                               \FROM voucher v, shop s \
                               \WHERE v.shop_id = s.id \
                               \AND v.timestamp LIKE '" ++ t ++ "%' \
                               \AND s.name LIKE '" ++ n ++ "%'" )
                             []
    return $ map (\(t:n:[]) -> wrapPair (DB.fromSql t) (DB.fromSql n)) result

mapVoucherCompletionToKey :: DataHandle
                          -> String
                          -> IO (Maybe Int)
mapVoucherCompletionToKey handle comp = do
    let (t,n) = unwrapPair comp
        (t',n') = (DB.toSql t, DB.toSql n)
    result <- DB.quickQuery' handle
                             "SELECT v.id \
                             \FROM voucher v, shop s \
                             \WHERE v.shop_id = s.id \
                             \AND v.timestamp = ? \
                             \AND s.name = ?"
                             [t', n']
    return $ justTopLeft result

mapVoucherKeyToCompletion :: DataHandle
                       -> Int
                       -> IO (Maybe String)
mapVoucherKeyToCompletion handle key = do
    result <- DB.quickQuery' handle
                             "SELECT v.timestamp, s.name \
                             \FROM voucher v, shop s \
                             \WHERE v.shop_id = s.id \
                             \AND v.id = ?"
                             params
    case result of
      ((timestamp:name:[]):[]) -> return $ Just
                                         $ wrapPair (DB.fromSql timestamp)
                                                    (DB.fromSql name)
  where params = [ DB.toSql key ]


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
                     -> IO [HKLC.Completion]
simpleWordCompletion fcomp word = do
    compList <- fcomp word
    case compList of
      [] -> return []
      _  -> do let compList' = map (drop $ length word) compList
                   prefix    = commonPrefix compList'
               return $ map (makeCompletion prefix) compList
  where makeCompletion [] comp = HKLC.Completion word comp False
        makeCompletion p comp  = HKLC.Completion (word ++ p) comp False

articleCompletion handle  = simpleWordCompletion (completeArticle handle)
categoryCompletion handle = simpleWordCompletion (completeCategory handle)
shopCompletion handle     = simpleWordCompletion (completeShop handle)
voucherCompletion handle  = simpleWordCompletion (completeVoucher handle)

