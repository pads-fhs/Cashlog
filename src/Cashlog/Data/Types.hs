module Cashlog.Data.Types where

data Article = Article {
      articleId :: Int
    , articleName :: String
    , articlePrice :: Double
    , articleCategoryId :: Int
    } deriving (Show)

type ArticleSkeleton = (String, Double, Int)

data Category = Category {
      categoryId :: Int
    , categoryParent :: Int
    , categoryName :: String
    } deriving (Show)

type CategorySkeleton = (Int, String)

data Shop = Shop {
      shopId :: Int
    , shopName :: String
    , shopCity :: String
    } deriving (Show)

type ShopSkeleton = (String, String)

data VoucherPosition = VoucherPosition {
      voucherPositionId :: Int
    , voucherPositionVoucherId :: Int
    , voucherPositionArticleId :: Int
    , voucherPositionQuantity :: Double
    , voucherPositionPrice :: Double
    } deriving (Show)

type VoucherPositionSkeleton = (Int, Int, Double, Double)

data Voucher = Voucher {
      voucherId :: Int
    , voucherTimestamp :: String
    , voucherShopId :: Int
    } deriving (Show)

type VoucherSkeleton = (String, Int)

