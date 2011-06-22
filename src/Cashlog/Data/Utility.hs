{-# LANGUAGE FlexibleContexts #-}
module Cashlog.Data.Utility where

import qualified Database.HDBC          as DB
import qualified Database.HDBC.SqlValue as SQV

import Data.Convertible.Base
import Data.List

justTopLeft :: Convertible SQV.SqlValue a
            => [[SQV.SqlValue]]
            -> Maybe a
justTopLeft r = case r of
                  ((x:[]):[]) -> Just $ DB.fromSql x
                  _           -> Nothing

wrapPair :: String
         -> String
         -> String
wrapPair n1 n2 = n1 ++ "(" ++ n2 ++ ")"

unwrapPair :: String
           -> (String,String)
unwrapPair p = let (n1,n2) = break (\b -> b == '(') p
               in  (n1, filter (\s -> not (s == '(' || s == ')')) n2)

