module Connection where

import Database.HDBC
import Database.HDBC.Sqlite3

connectDatabase :: FilePath
                -> IO Connection
connectDatabase path = do
    con <- connectSqlite3 path
    run con "PRAGMA case_sensitive_like = TRUE" []
    return con

disconnectDatabase :: Connection
                   -> IO ()
disconnectDatabase = disconnect
