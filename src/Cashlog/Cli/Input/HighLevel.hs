module Cashlog.Cli.Input.HighLevel where

import Data.Time.Clock

import Cashlog.Data.Connection
import Cashlog.Data.Access
import Cashlog.Data.Completion
import Cashlog.Data.Types
import Cli.Input.LowLevel

readArticleKey :: DataHandle
               -> String
               -> Maybe Int
               -> IO Int
readArticleKey handle msg def = readKey msg
                                        def
                                        (articleCompletion handle)
                                        (mapArticleKeyToCompletion handle)
                                        (mapArticleCompletionToKey handle)
                                        (isArticleKey handle)

readCategoryKey :: DataHandle
                -> String
                -> Maybe Int
                -> IO Int
readCategoryKey handle msg def = readKey msg
                                         def
                                         (categoryCompletion handle)
                                         (mapCategoryKeyToCompletion handle)
                                         (mapCategoryCompletionToKey handle)
                                         (isCategoryKey handle)

readShopKey :: DataHandle
            -> String
            -> Maybe Int
            -> IO Int
readShopKey handle msg def = readKey msg
                                     def
                                     (shopCompletion handle)
                                     (mapShopKeyToCompletion handle)
                                     (mapShopCompletionToKey handle)
                                     (isShopKey handle)

readArticle :: DataHandle
            -> Maybe Article
            -> IO ArticleSkeleton 
readArticle handle article = do
    name  <- readString "Bezeichnung"           $ fmap articleName article
    price <- readValue  "Preis"                 $ fmap articlePrice article
    catId <- readCategoryKey handle "Kategorie" $ fmap articleCategoryId article
    return (name, price, catId)

readCategory :: Maybe Category
             -> IO CategorySkeleton
readCategory cat = do
    name <- readString "Bezeichnung" $ fmap categoryName cat
    return (0, name)

readShop :: Maybe Shop ->
            IO ShopSkeleton
readShop shop = do
    name <- readString "Name"  $ fmap shopName shop
    city <- readString "Stadt" $ fmap shopCity shop
    return (name, city)

readVoucher :: DataHandle
            -> IO VoucherSkeleton
readVoucher handle = do
    def       <- getCurrentTime
    timestamp <- readDateTime (Just def)
    shop      <- readShopKey handle "Geschäft" Nothing
    return (timestamp, shop)

readVoucherPosition :: DataHandle
                    -> Int
                    -> IO VoucherPositionSkeleton
readVoucherPosition handle vouKey = do
    artKey   <- readArticleKey handle "Artikel" Nothing
    artPrice <- unsafeGetArticlePrice handle artKey
    quantity <- readValue "Menge" (Just 1.0)
    price    <- readValue "Preis" (Just $ (artPrice * quantity))
    return (vouKey, artKey, quantity, price)
