module Cashlog.Data.Connection where

import qualified Database.HDBC         as DB
import qualified Database.HDBC.Sqlite3 as SQL3

type DataHandle = SQL3.Connection

connectDataSource :: FilePath
                  -> IO DataHandle
connectDataSource path = do
    con <- SQL3.connectSqlite3 path
    DB.run con "PRAGMA case_sensitive_like = TRUE" []
    return con

disconnectDataSource :: DataHandle
                     -> IO ()
disconnectDataSource = DB.disconnect

