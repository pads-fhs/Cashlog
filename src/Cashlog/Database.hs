module Cashlog.Database where

import Database.HDBC
import Database.HDBC.Sqlite3
import System.Time

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> insertArticle
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

insertArticle :: Connection -> 
                 Article -> 
                 IO ()
insertArticle con a = do
    run con "INSERT INTO article VALUES (?, ?, ?, ?)" param
    commit con
    where param = [ SqlNull
                  , toSql $ articleId a
                  , toSql $ articlePrice a
                  , toSql $ articleCategoryId a
                  ]

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> updateArticle
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

updateArticle :: Connection -> 
                 Article -> 
                 IO ()
updateArticle con a = do
	run con "UPDATE article SET name = ?, price = ?, cat_id = ?, WHERE id = ?)" param
	commit con
    where param = [ toSql $ articleName a
                  , toSql $ articlePrice a
                  , toSql $ articleCategoryId a
                  , toSql $ articleId a
                  ]

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> deleteArticle
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

deleteArticle :: Connection -> 
                  Article -> 
                  IO ()
deleteArticle con a = do
	run con "DELETE FROM article WHERE id = ?" param
	commit con
    where param = [ toSql $ articleId a ]


-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> selectArticles
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selectArticles :: Connection -> 
                  IO [ Article ]
selectArticles con = do
    queryResult <- quickQuery' con "SELECT * FROM article" []
    return $ map conv queryResult
    where conv (i:n:p:c:[]) = Article  
                              ( fromSql i :: Int )
                              ( fromSql n :: String )
                              ( fromSql p :: Double )
                              ( fromSql c :: Int )

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> insertCategory
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

insertCategory :: Connection -> 
                  Category -> 
                  IO ()
insertCategory con c = do
	run con "INSERT INTO category VALUES (?, ?, ?)" param
	commit con
    where param = [ SqlNull
                  , toSql $ categoryParent c
                  , toSql $ categoryName c
                  ]

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> updateCategory
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

updateCategory :: Connection -> 
                  Category -> 
                  IO ()
updateCategory con c = do
	run con "UPDATE category SET parent = ?, name = ? WHERE id = ?" param
	commit con
	where param = [ toSql (0 :: Int)
                  , toSql $ categoryName c
                  , toSql $ categoryId c
                  ]

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> deleteCatgory
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

deleteCategory :: Connection ->
                  Category ->
                  IO ()
deleteCategory con c = do
    run con "DELETE FROM category WHERE id = ?" param
    commit con
    where param = [ toSql $ categoryId c ]

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> selectCategories
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selectCategories :: Connection ->
                    IO [ Category ]
selectCategories con = do
    queryResult <- quickQuery' con "SELECT * FROM category" []
    return $ map conv queryResult
    where conv (i:p:n:[]) = Category
                            ( fromSql i :: Int )
                            ( fromSql p :: Int )
                            ( fromSql n :: String )

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> insertShop
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

insertShop :: Connection ->
              Shop ->
              IO ()
insertShop con s = do
    run con "INSERT INTO shop VALUES(?, ?, ?)" params
    commit con
    where params = [ toSql $ shopId s
                   , toSql $ shopName s
                   , toSql $ shopCity s
                   ]

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> updateShop
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

updateShop :: Connection ->
              Shop ->
              IO ()
updateShop con s = do
    run con "UPDATE shop SET name = ?, city = ? WHERE id = ?" param
    commit con
    where param = [ toSql $ shopName s
                  , toSql $ shopCity s
                  , toSql $ shopId s
                  ]

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> deleteShop
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

deleteShop :: Connection ->
              Shop ->
              IO ()
deleteShop con s = do
    run con "DELETE FROM shop WHERE id = ?" param
    commit con
    where param = [ toSql $ shopId s ]

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> selectShops
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

selectShops :: Connection ->
               IO [ Shop ]
selectShops con = do
    queryResult <- quickQuery' con "SELECT * FROM shop" []
    return $ map conv queryResult
    where conv (i:n:c:[]) = Shop
                            ( fromSql i :: Int )
                            ( fromSql n :: String )
                            ( fromSql c :: String )

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> insertVoucher
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

insertVoucher :: Connection ->
                 Voucher ->
                 IO Int
insertVoucher con v = do
    maxId <- selectMaxId
    let maxId' = maxId + 1
        param = [ toSql maxId'
                , toSql $ voucherTimestamp v
                , toSql $ voucherShopId v
                ]
    run con "INSERT INTO voucher VALUES(?, ?, ?)" param
    commit con
    return maxId'
    where selectMaxId = do r <- quickQuery' con "SELECT max(id) FROM voucher" []
                           return ( fromSql $ head . head $ r :: Int )

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> updateVoucher
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

updateVoucher :: Connection ->
                 Voucher ->
                 IO ()
updateVoucher con v = do
    run con "UPDATE voucher SET timestamp = ?, shop_id = ? WHERE id = ?" params
    commit con
    where params = [ toSql $ voucherTimestamp v
                   , toSql $ voucherShopId v
                   , toSql $ voucherId
                   ]

-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> deleteVoucher
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

deleteVoucher :: Connection ->
                 Voucher ->
                 IO ()
deleteVoucher con v = do
    run con "DELETE FROM voucher WHERE id = ?" params
    commit con
    where params = [ toSql $ voucherId v ]


-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> selectVouchersByDate
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
{-
selectVouchersByDate :: Connection ->
                        ClockTime ->
                        IO [ Voucher ]
selectVouchersByDate con t = do
    return []
-}
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
-- >>>>> insertVoucherPosition
-- >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

