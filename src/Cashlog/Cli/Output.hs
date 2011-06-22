module Cashlog.Cli.Output where

import Text.Printf

import Cashlog.Data.Connection
import Cashlog.Data.Access
import Cashlog.Cli.Utility

printArticles :: DataHandle
              -> IO ()
printArticles handle = do
    articles <- prettySelectArticles handle
    printf "%s | %s | %s" "Bezeichnung" "Vorgabepreis" "Kategorie"
    mapM_ (\(name, price, catName) -> printf "%s | %d | %s" name price catName)
          articles

printCategories :: DataHandle
                -> IO ()
printCategories handle = do
    categories <- prettySelectCategories handle
    printf "%s | %s" "Nummer" "Bezeichnung"
    mapM_ (\(id, name) -> printf "%d | %s" id name) categories

printShops :: DataHandle
           -> IO ()
printShops handle = do
    shops <- prettySelectShops handle
    printf "%s | %s" "Geschäft" "Ort"
    mapM_ (\(name, city) -> printf "%s | %s" name city) shops

printVouchers :: DataHandle
              -> String
              -> IO ()
printVouchers handle date = do
    vouchers <- prettySelectVouchers handle dateFormat date
    printf "%s | %s" "Datum / Zeit" "Geschäft"
    mapM_ (\(timestamp, shopName) -> printf "%s | %s" timestamp shopName)
          vouchers

printVoucherPositions :: DataHandle
                      -> Int
                      -> IO ()
printVoucherPositions handle key = do
    positions <- prettySelectVoucherPositions handle key
    let sumPrices = foldl (\a (_, _, p) -> a + p) 0 positions
    printf "%s | %s | %s" "Artikel" "Menge" "Preis"
    mapM_ (\(artName, posQuantity, posPrice) -> printf "%s | %d | %d"
                                                       artName
                                                       posQuantity
                                                       posPrice)
          positions
    putStrLn ("Gesamtpreis: " ++ (show sumPrices))

