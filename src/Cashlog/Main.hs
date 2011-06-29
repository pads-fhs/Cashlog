module Main where
import System.IO(hSetBuffering, stdin, stdout, BufferMode(NoBuffering))

import Cashlog.Data.Connection
import Cashlog.Cli.Dialog

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    handle <- connectDataSource "cashlog.db"
    mainloop handle
    disconnectDataSource handle

mainloop :: DataHandle
         -> IO ()
mainloop handle = do
    m <- mainMenu
    case m of
      'a'       -> do s <- subMenu
                      case s of
                        'a' -> articleDialog handle Insert
                        'b' -> articleDialog handle Update
                        'l' -> articleDialog handle Delete
                        'p' -> articleDialog handle List
                        'q' -> return ()
                      mainloop handle >> return ()
      'k'       -> do s <- subMenu
                      case s of
                        'a' -> categoryDialog handle Insert
                        'b' -> categoryDialog handle Update
                        'l' -> categoryDialog handle Delete
                        'p' -> categoryDialog handle List
                        'q' -> return ()
                      mainloop handle >> return ()
      'g'       -> do s <- subMenu
                      case s of
                        'a' -> shopDialog handle Insert
                        'b' -> shopDialog handle Update
                        'l' -> shopDialog handle Delete
                        'p' -> shopDialog handle List
                        'q' -> return ()
                      mainloop handle >> return ()
      'b'       -> do s <- subMenu'
                      case s of
                        'a' -> voucherDialog handle Insert
                        'p' -> voucherDialog handle List
                        'q' -> mainMenu >> return ()
                      mainloop handle >> return ()
      'p'       -> do positionDialog handle List
                      mainloop handle >> return ()
      'q'       -> return ()
      otherwise -> mainloop handle

mainMenu :: IO Char
mainMenu = do
    putStrLn "********************************************************************************"
    putStrLn "** a - Artikel                                                                **"
    putStrLn "** k - Kategorien                                                             **"
    putStrLn "** g - Geschäfte                                                              **"
    putStrLn "** b - Belege                                                                 **"
    putStrLn "** p - Positionen                                                             **"
    putStrLn "** q - Beenden                                                                **"
    putStrLn "********************************************************************************"
    putStr "Eingabe: "
    c <- getChar
    putStr "\n"
    return c

subMenu :: IO Char
subMenu = do
    putStrLn "********************************************************************************"
    putStrLn "** a - Anlegen                                                                **"
    putStrLn "** b - Bearbeiten                                                             **"
    putStrLn "** l - Löschen                                                                **"
    putStrLn "** p - Ausgeben                                                               **"
    putStrLn "** q - Zurück                                                                 **"
    putStrLn "********************************************************************************"
    putStr "Eingabe: "
    c <- getChar
    putStr "\n"
    if elem c "ablpq"
     then return c
     else subMenu

subMenu' :: IO Char
subMenu' = do
    putStrLn "********************************************************************************"
    putStrLn "** a - Anlegen                                                                **"
    putStrLn "** p - Ausgeben                                                               **"
    putStrLn "** q - Zurück                                                                 **"
    putStrLn "********************************************************************************"
    putStr "Eingabe: "
    c <- getChar
    putStr "\n"
    if elem c "apq"
     then return c
     else subMenu
