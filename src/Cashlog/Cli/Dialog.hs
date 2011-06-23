module Cashlog.Cli.Dialog where

import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format
import System.Locale
import Data.Maybe

import Cashlog.Data.Connection
import Cashlog.Data.Access
import Cashlog.Cli.Input.LowLevel
import Cashlog.Cli.Input.HighLevel
import Cashlog.Cli.Output
import Cashlog.Cli.Utility

data Action = Insert
            | Update
            | Delete
            | List

printHeader :: String
            -> Action
            -> IO ()
printHeader title action = do
    putStr "++++ "
    putStr title
    putStr $ case action of
               Insert -> " hinzufügen "
               Update -> " bearbeiten "
               Delete -> " löschen    "
               List   -> " anzeigen   "
    putStrLn $ replicate (63 - (length title)) '+'

printFooter :: IO ()
printFooter = putStrLn $ replicate 80 '+'

articleDialog :: DataHandle
              -> Action
              -> IO ()
articleDialog handle action = do
    printHeader "Artikel" action
    case action of
      Insert -> do articleSkeleton <- readArticle handle Nothing
                   insertArticle handle articleSkeleton
      Update -> do articleKey      <- readArticleKey handle "Artikel" Nothing
                   article         <- selectArticle handle articleKey
                   articleSkeleton <- readArticle handle article
                   updateArticle handle articleKey articleSkeleton
      Delete -> do articleKey <- readArticleKey handle "Artikel" Nothing
                   deleteArticle handle articleKey
      List   -> do printArticles handle
    printFooter

categoryDialog :: DataHandle
               -> Action
               -> IO ()
categoryDialog handle action = do
    printHeader "Kategorie" action
    case action of
      Insert -> do categorySkeleton <- readCategory Nothing
                   insertCategory handle categorySkeleton
      Update -> do categoryKey      <- readCategoryKey handle "Kategorie" Nothing
                   category         <- selectCategory handle categoryKey
                   categorySkeleton <- readCategory category
                   updateCategory handle categoryKey categorySkeleton
      Delete -> do categoryKey <- readCategoryKey handle "Kategorie" Nothing
                   deleteCategory handle categoryKey
      List   -> do printCategories handle
    printFooter

shopDialog :: DataHandle
           -> Action
           -> IO ()
shopDialog handle action = do
    printHeader "Geschäft" action
    case action of
      Insert -> do shopSkeleton <- readShop Nothing
                   insertShop handle shopSkeleton
      Update -> do shopKey      <- readShopKey handle "Geschäft" Nothing
                   shop         <- selectShop handle shopKey
                   shopSkeleton <- readShop shop
                   updateShop handle shopKey shopSkeleton
      Delete -> do shopKey <- readShopKey handle "Geschäft" Nothing
                   deleteShop handle shopKey
      List   -> do printShops handle
    printFooter

voucherDialog :: DataHandle
              -> Action
              -> IO ()
voucherDialog handle action = do
    printHeader "Beleg" action
    case action of
      Insert    -> do voucherSkeleton <- readVoucher handle
                      voucherKey      <- insertVoucher handle voucherSkeleton
                      putStrLn "insert fertig"
                      posList         <- readVoucherPositions handle voucherKey
                      putStrLn "pos lesen fertig"
                      mapM_ (insertVoucherPosition handle) posList
                      putStrLn "pos insert fertig"
      List      -> do defDate <- getCurrentTime
                      date    <- readDate "Datum" (Just $ utctDay defDate)
                      let date' = formatTime defaultTimeLocale dateFormat date
                      printVouchers handle date'
      otherwise -> putStrLn "Operation nicht zulässig"
    printFooter

positionDialog :: DataHandle
               -> Action
               -> IO ()
positionDialog handle action = do
    printHeader "Positionen" action
    case action of
      List      -> do voucherKey <- readVoucherKey handle "Beleg" Nothing
                      printVoucherPositions handle voucherKey
      otherwise -> putStrLn "Operation nicht zulässig"
    printFooter
