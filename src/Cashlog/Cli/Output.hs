module Cashlog.Cli.Output where

import Text.Printf

import Cashlog.Data.Connection
import Cashlog.Data.Access
import Cashlog.Cli.Utility

printArticles :: DataHandle
              -> IO ()
printArticles handle = do
    articles <- prettySelectArticles handle
    printf "%24s | %24s | %24s\n" "Bezeichnung" "Vorgabepreis" "Kategorie"
    putStrLn $ replicate 80 '-'
    mapM_ (\(name, price, catName) -> printf "%24s | %24.2f | %24s\n" name price catName)
          articles

printCategories :: DataHandle
                -> IO ()
printCategories handle = do
    categories <- prettySelectCategories handle
    printf "%24s | %24s\n" "Nummer" "Bezeichnung"
    putStrLn $ replicate 80 '-'
    mapM_ (\(id, name) -> printf "%24.2f | %24s\n" id name) categories

printShops :: DataHandle
           -> IO ()
printShops handle = do
    shops <- prettySelectShops handle
    printf "%24s | %24s\n" "Geschäft" "Ort"
    putStrLn $ replicate 80 '-'
    mapM_ (\(name, city) -> printf "%24s | %24s\n" name city) shops

printVouchers :: DataHandle
              -> String
              -> IO ()
printVouchers handle date = do
    vouchers <- prettySelectVouchers handle dateFormat date
    printf "%24s | %24s\n" "Datum / Zeit" "Geschäft"
    putStrLn $ replicate 80 '-'
    mapM_ (\(timestamp, shopName) -> printf "%24s | %24s\n" timestamp shopName)
          vouchers

printVoucherPositions :: DataHandle
                      -> Int
                      -> IO ()
printVoucherPositions handle key = do
    positions <- prettySelectVoucherPositions handle key
    let sumPrices = foldl (\a (_, _, p) -> a + p) 0 positions
    printf "%24s | %24s | %24s\n" "Artikel" "Menge" "Preis"
    putStrLn $ replicate 80 '-'
    mapM_ (\(artName, posQuantity, posPrice) -> 
              printf "%24s | %24.2f | %24.2f\n"
                     artName
                     posQuantity
                     posPrice)
          positions
    putStrLn ("Gesamtpreis: " ++ (show sumPrices))

