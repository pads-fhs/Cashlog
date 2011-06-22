module Cashlog.Cli.Input.LowLevel
    ( readString
    , readDateTime
    , readValue
    , readKey
    ) where

import Control.Exception
import qualified System.Console.Haskeline            as HKL
import qualified System.Console.Haskeline.Completion as HKLC
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format hiding (readTime)
import System.Locale

import Cashlog.Data.Types
import Cashlog.Data.Completion

readString :: String
           -> Maybe String
           -> IO String
readString msg def = HKL.runInputT settings loop
  where settings = HKL.Settings HKLC.noCompletion Nothing False
        loop     = do raw <- case def of
                               Just d  -> HKL.getInputLineWithInitial (msg ++ ": ") (d, "")
                               Nothing -> HKL.getInputLine (msg ++ ": ")
                      case raw of
                        Just r  -> return r
                        Nothing -> loop

dateFormat = "%d.%m.%Y"
timeFormat = "%R"
iso8601SqlFormat = "%Y-%m-%d %T"

readDate :: String
         -> Maybe Day
         -> IO Day
readDate msg def = do
    d <- readString msg defDate
    case (parseTime defaultTimeLocale dateFormat d) of
      Just d' -> return d'
      Nothing -> readDate msg def
  where defDate = fmap (formatTime defaultTimeLocale dateFormat) def

readTime :: String
         -> Maybe TimeOfDay
         -> IO TimeOfDay
readTime msg def = do
    t <- readString msg defTime
    case parseTime defaultTimeLocale timeFormat t of
      Just t' -> return t'
      Nothing -> readTime msg def
  where defTime = fmap (formatTime defaultTimeLocale timeFormat) def

readDateTime :: Maybe UTCTime
             -> IO String
readDateTime def = do
    d <- readDate "Datum"   $ fmap utctDay def
    t <- readTime "Uhrzeit" $ fmap timeToTimeOfDay (fmap utctDayTime def)
    return $ formatTime defaultTimeLocale iso8601SqlFormat (UTCTime d (timeOfDayToTime t))

readValue :: (Read a, Show a)
          => String
          -> Maybe a
          -> IO a
readValue msg def = do
    raw <- readString msg $ fmap show def
    tryRead <- try $ readIO raw :: Read a => IO ( Either SomeException a )
    case tryRead of
      Left _ -> readValue msg def
      Right val -> return val

readKey :: String
        -> Maybe Int
        -> (String -> IO [HKLC.Completion])
        -> (Int -> IO (Maybe String))
        -> (String -> IO (Maybe Int))
        -> (Int -> IO Bool)
        -> IO Int
readKey msg def fcomp fmapcomp fmapkey fchkkey = do
    defStr <- case def of
                Just defInt -> do defStr' <- fmapcomp defInt
                                  case defStr' of
                                    Just s  -> return s
                                    Nothing -> return $ show defInt
                Nothing     -> return ""
    inp <- HKL.runInputT (HKL.Settings (HKLC.completeWord Nothing " " fcomp) Nothing False) (loop defStr)
    tryReadInt <- try $ readIO inp :: IO (Either SomeException Int)
    case tryReadInt of
      Left _    -> do mkkey <- fmapkey inp
                      case mkkey of
                        Just key -> return key
                        Nothing  -> readKey msg def fcomp fmapcomp fmapkey fchkkey
      Right key -> do chkkey <- fchkkey key
                      case chkkey of
                        True -> return key
                        _    -> readKey msg def fcomp fmapcomp fmapkey fchkkey
  where loop def' = do
            raw <- HKL.getInputLineWithInitial (msg ++ ": ") (def', "")
            case raw of
              Just r  -> return r
              Nothing -> loop def'
