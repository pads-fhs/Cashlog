module Cashlog.Database where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Time

import Types

insertArticle :: Connection -> 
                 Article -> 
                 IO ()
insertArticle con a = do
    run con "INSERT INTO article VALUES (?, ?, ?, ?)" param
    commit con
    where param = [ SqlNull
                  , toSql $ articleId a
                  , toSql $ articlePrice a
                  , toSql $ articleCategoryId a ]

updateArticle :: Connection -> 
                 Article -> 
                 IO ()
updateArticle con a = do
	run con "UPDATE article SET name = ?, price = ?, cat_id = ?, WHERE id = ?)" param
	commit con
  where param = [ toSql $ articleName a
                , toSql $ articlePrice a
                , toSql $ articleCategoryId a
                , toSql $ articleId a ]

deleteArticle :: Connection -> 
                  Article -> 
                  IO ()
deleteArticle con a = do
	run con "DELETE FROM article WHERE id = ?" param
	commit con
  where param = [ toSql $ articleId a ]

selectArticles :: Connection -> 
                  IO [ Article ]
selectArticles con = do
    queryResult <- quickQuery' con "SELECT * FROM article" []
    return $ map conv queryResult
  where conv (i:n:p:c:[]) = Article (fromSql i)
                                    (fromSql n)
                                    (fromSql p)
                                    (fromSql c)

insertCategory :: Connection -> 
                  Category -> 
                  IO ()
insertCategory con c = do
	run con "INSERT INTO category VALUES (?, ?, ?)" param
	commit con
  where param = [ SqlNull
                , toSql $ categoryParent c
                , toSql $ categoryName c ]

updateCategory :: Connection -> 
                  Category -> 
                  IO ()
updateCategory con c = do
	run con "UPDATE category SET parent = ?, name = ? WHERE id = ?" param
	commit con
  where param = [ toSql (0 :: Int)
                , toSql $ categoryName c
                , toSql $ categoryId c ]

deleteCategory :: Connection ->
                  Category ->
                  IO ()
deleteCategory con c = do
    run con "DELETE FROM category WHERE id = ?" param
    commit con
  where param = [ toSql $ categoryId c ]

selectCategories :: Connection ->
                    IO [ Category ]
selectCategories con = do
    queryResult <- quickQuery' con "SELECT * FROM category" []
    return $ map conv queryResult
  where conv (i:p:n:[]) = Category (fromSql i)
                                   (fromSql p)
                                   (fromSql n)

insertShop :: Connection ->
              Shop ->
              IO ()
insertShop con s = do
    run con "INSERT INTO shop VALUES(?, ?, ?)" params
    commit con
  where params = [ toSql $ shopId s
                 , toSql $ shopName s
                 , toSql $ shopCity s ]

updateShop :: Connection ->
              Shop ->
              IO ()
updateShop con s = do
    run con "UPDATE shop SET name = ?, city = ? WHERE id = ?" param
    commit con
  where param = [ toSql $ shopName s
                , toSql $ shopCity s
                , toSql $ shopId s ]

deleteShop :: Connection ->
              Shop ->
              IO ()
deleteShop con s = do
    run con "DELETE FROM shop WHERE id = ?" param
    commit con
  where param = [ toSql $ shopId s ]

selectShops :: Connection ->
               IO [ Shop ]
selectShops con = do
    queryResult <- quickQuery' con "SELECT * FROM shop" []
    return $ map conv queryResult
  where conv (i:n:c:[]) = Shop (fromSql i)
                               (fromSql n)
                               (fromSql c)

insertVoucher :: Connection ->
                 Voucher ->
                 IO Int
insertVoucher con v = do
    maxId <- selectMaxId
    let maxId' = maxId + 1
        param = [ toSql maxId'
                , toSql $ voucherTimestamp v
                , toSql $ voucherShopId v ]
    run con "INSERT INTO voucher VALUES(?, ?, ?)" param
    commit con
    return maxId'
  where selectMaxId = do r <- quickQuery' con "SELECT max(id) FROM voucher" []
                         return ( fromSql $ head . head $ r :: Int )

updateVoucher :: Connection ->
                 Voucher ->
                 IO ()
updateVoucher con v = do
    run con "UPDATE voucher SET timestamp = ?, shop_id = ? WHERE id = ?" params
    commit con
  where params = [ toSql $ voucherTimestamp v
                 , toSql $ voucherShopId v
                 , toSql $ voucherId v ]

deleteVoucher :: Connection ->
                 Voucher ->
                 IO ()
deleteVoucher con v = do
    run con "DELETE FROM voucher WHERE id = ?" params
    commit con
  where params = [ toSql $ voucherId v ]

selectVouchersByYear :: Connection ->
                        Int ->
                        IO [ Voucher ]
selectVouchersByYear con t = do
    queryResult <- quickQuery' con "SELECT * FROM voucher WHERE strftime('%Y', timestamp) = ?" params
    return $ map conv queryResult
  where params = [ toSql t ]
        conv (i:t:s:[]) = Voucher (fromSql i)
                                  (fromSql t)
                                  (fromSql s)

selectVouchersByYearAndMonth :: Connection ->
                                Int ->
                                Int ->
                                IO [Voucher]
selectVouchersByYearAndMonth con y m = do
    queryResult <- quickQuery' con "SELECT * FROM voucher WHERE strftime('%Y', timestamp) = ? AND strftime('%m', timestamp) = ?" params
    return $ map conv queryResult
  where params = [ toSql y
                 , toSql m ]
        conv (i:t:s:[]) = Voucher (fromSql i)
                                  (fromSql t)
                                  (fromSql s)

insertPosition :: Connection ->
                  VoucherPosition ->
                  IO ()
insertPosition con p = do
    run con "INSERT INTO position VALUES(?, ?, ?, ?, ?)" params
    commit con
    return ()
  where params = [ SqlNull
                 , toSql $ voucherPositionVoucherId p
                 , toSql $ voucherPositionArticleId p
                 , toSql $ voucherPositionQuantity p
                 , toSql $ voucherPositionPrice p ]

updatePosition :: Connection ->
                  VoucherPosition ->
                  IO ()
updatePosition con p = do
    run con "UPDATE position SET voucher_id = ?, article_id = ?, quantity = ?, price = ? WHERE id = ?" params
    commit con
  where params = [ toSql $ voucherPositionVoucherId p
                 , toSql $ voucherPositionVoucherId p
                 , toSql $ voucherPositionQuantity p
                 , toSql $ voucherPositionPrice p
                 , toSql $ voucherPositionId p ]

deletePosition :: Connection ->
                  VoucherPosition ->
                  IO ()
deletePosition con p = do
    run con "DELETE FROM position WHERE id = ?" params
    commit con
  where params = [ toSql $ voucherPositionId p ]

selectPositions :: Connection ->
                   Voucher ->
                   IO [VoucherPosition]
selectPositions con v = do
    queryResult <- quickQuery' con "SELECT * FROM position WHERE voucher_id = ?" params
    return $ map conv queryResult
  where params = [ toSql $ voucherId v ]
        conv (i:v:a:q:p:[]) = VoucherPosition (fromSql i)
                                              (fromSql v)
                                              (fromSql a)
                                              (fromSql q)
                                              (fromSql p)
