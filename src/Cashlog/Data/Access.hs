module Cashlog.Data.Access where

import qualified Database.HDBC          as DB
import Data.Maybe

import Cashlog.Data.Connection
import Cashlog.Data.Types
import Cashlog.Data.Utility

-- -----------------------------------------------------------------------------
-- -- Article ------------------------------------------------------------------
-- -----------------------------------------------------------------------------

insertArticle :: DataHandle
              -> ArticleSkeleton
              -> IO ()
insertArticle handle (name, price, catId) = do
    DB.run handle
           "INSERT INTO article VALUES(?, ?, ?, ?)"
           params
    DB.commit handle
  where params = [ DB.SqlNull
                 , DB.toSql name
                 , DB.toSql price
                 , DB.toSql catId
                 ]

updateArticle :: DataHandle
              -> Int
              -> ArticleSkeleton
              -> IO ()
updateArticle handle key (name ,price, catId) = do
    DB.run handle 
           "UPDATE article SET name = ?, price = ?, cat_id = ?, WHERE id = ?)"
           params
    DB.commit handle
  where params = [ DB.toSql key
                 , DB.toSql name
                 , DB.toSql price
                 , DB.toSql catId
                 ]

deleteArticle :: DataHandle
              -> Int
              -> IO ()
deleteArticle handle key = do
    DB.run handle
           "DELETE FROM article WHERE id = ?"
           params
    DB.commit handle
  where params = [ DB.toSql key ]

selectArticle :: DataHandle
              -> Int
              -> IO (Maybe Article)
selectArticle handle key = do
    result <- DB.quickQuery' handle
                             "SELECT * from article WHERE id = ?"
                             [DB.toSql key]
    case result of
      ((id:name:price:catId:[]):[]) -> return $ Just
                                              $ Article (DB.fromSql id)
                                                        (DB.fromSql name)
                                                        (DB.fromSql price)
                                                        (DB.fromSql catId)
      otherwise                     -> return Nothing

unsafeGetArticlePrice :: DataHandle
                      -> Int
                      -> IO Double
unsafeGetArticlePrice handle key = do
    result <- DB.quickQuery' handle
                             "SELECT price FROM article WHERE id = ?"
                             [DB.toSql key]
    return $ fromJust $ justTopLeft $ result

prettySelectArticles :: DataHandle
                     -> IO [(String, Double, String)]
prettySelectArticles handle = do
    result <- DB.quickQuery' handle
                             "SELECT a.name, a.price, c.name \
                             \FROM article a, category c \
                             \WHERE a.category_id = c.id"
                             []
    return $ map (\(artName:artPrice:catName:[]) -> ( DB.fromSql artName
                                                    , DB.fromSql artPrice
                                                    , DB.fromSql catName
                                                    )) 
                 result

-- -----------------------------------------------------------------------------
-- -- Category -----------------------------------------------------------------
-- -----------------------------------------------------------------------------

insertCategory :: DataHandle
               -> CategorySkeleton
               -> IO ()
insertCategory handle (_, name) = do
    DB.run handle 
           "INSERT INTO category VALUES(?, ?, ?)"
           params
    DB.commit handle
  where params = [ DB.SqlNull
                 , DB.toSql (0 :: Int)
                 , DB.toSql name
                 ]

updateCategory :: DataHandle
               -> Int
               -> CategorySkeleton
               -> IO ()
updateCategory handle key (_, name) = do
    DB.run handle
           "UPDATE article SET name = ? WHERE id = ?"
           params
    DB.commit handle
  where params = [ DB.toSql name
                 , DB.toSql key
                 ]

deleteCategory :: DataHandle
               -> Int
               -> IO ()
deleteCategory handle key = do
    DB.run handle
           "DELETE FROM category WHERE id = ?"
           [DB.toSql key]
    DB.commit handle

prettySelectCategories :: DataHandle
                       -> IO [(Int, String)]
prettySelectCategories handle = do
    result <- DB.quickQuery' handle
                             "SELECT id, name FROM category"
                             []
    return $ map (\(id:name:[]) -> ( DB.fromSql id
                                   , DB.fromSql name
                                   ))
                 result

-- -----------------------------------------------------------------------------
-- -- Shop ---------------------------------------------------------------------
-- -----------------------------------------------------------------------------

insertShop :: DataHandle
           -> ShopSkeleton
           -> IO ()
insertShop handle (name, city) = do
    DB.run handle
           "INSERT INTO shop VALUES(?, ?, ?)"
           params
    DB.commit handle
  where params = [ DB.SqlNull
                 , DB.toSql name
                 , DB.toSql city
                 ]

updateShop :: DataHandle
           -> Int
           -> ShopSkeleton
           -> IO ()
updateShop handle key (name, city) = do
    DB.run handle
           "UPDATE shop SET name = ?, city = ? WHERE id = ?"
           params
    DB.commit handle
  where params = [ DB.toSql name
                 , DB.toSql city
                 , DB.toSql key
                 ]

deleteShop :: DataHandle
           -> Int
           -> IO ()
deleteShop handle key = do
    DB.run handle
           "DELETE FROM shop WHERE id = ?"
           [DB.toSql key]
    DB.commit handle

prettySelectShops :: DataHandle
                  -> IO [(String, String)]
prettySelectShops handle = do
    result <- DB.quickQuery' handle
                             "SELECT name, city FROM shop"
                             []
    return $ map (\(name:city:[]) -> ( DB.fromSql name
                                     , DB.fromSql city
                                     ))
                 result

-- -----------------------------------------------------------------------------
-- -- Voucher ------------------------------------------------------------------
-- -----------------------------------------------------------------------------

insertVoucher :: DataHandle
              -> VoucherSkeleton
              -> IO ()
insertVoucher handle (timestamp, shopId) = do
    DB.run handle 
           "INSERT INTO voucher VALUES(?, ?, ?)"
           params
    DB.commit handle
  where params = [ DB.SqlNull
                 , DB.toSql timestamp
                 , DB.toSql shopId
                 ]

prettySelectVouchers :: DataHandle
                     -> String
                     -> String
                     -> IO [(String, String)]
prettySelectVouchers handle format timestamp = do
    result <- DB.quickQuery' handle 
                             ( "SELECT v.timestamp, s.name \
                               \FROM voucher v, shop s \
                               \WHERE v.shop_id = s.id \
                               \AND strftime('" ++ format ++ "', \
                               \v.timestamp) = ?" )
                             [DB.toSql timestamp]
    return $ map (\(timestamp:shopName:[]) -> ( DB.fromSql timestamp
                                              , DB.fromSql shopName
                                              ))
                 result

-- -----------------------------------------------------------------------------
-- -- VoucherPosition ----------------------------------------------------------
-- -----------------------------------------------------------------------------

insertVoucherPosition :: DataHandle
                      -> VoucherPositionSkeleton
                      -> IO ()
insertVoucherPosition handle (vouId, artId, quantity, price) = do
    DB.run handle
           "INSERT INTO position VALUES(?, ?, ?. ?. ?)"
           params
    DB.commit handle
  where params = [ DB.SqlNull
                 , DB.toSql vouId
                 , DB.toSql artId
                 , DB.toSql quantity
                 , DB.toSql price
                 ]

prettySelectVoucherPositions :: DataHandle
                             -> Int
                             -> IO [(String, Double, Double)]
prettySelectVoucherPositions handle vouKey = do
    result <- DB.quickQuery' handle
                             "SELECT a.name, p.quantity, p.price \
                             \FROM article a, position p \
                             \WHERE p.id = ? AND p.article_id = a.id"
                             [DB.toSql vouKey]
    return $ map (\(artName:posQuantity:posPrice:[]) -> ( DB.fromSql artName
                                                        , DB.fromSql posQuantity
                                                        , DB.fromSql posPrice
                                                        ))
                 result

