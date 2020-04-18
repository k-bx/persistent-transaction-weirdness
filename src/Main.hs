{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Control.Concurrent
import Control.Monad.Logger (MonadLogger (..), fromLogStr, toLogStr)
import Control.Monad.Trans.Reader
import Data.Pool
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH
import UnliftIO

-- | 'P.withPostgresqlPool' needs this.
instance MonadLogger IO where
  monadLoggerLog _loc _logSource _logLevel msg = do
    tid <- Control.Concurrent.myThreadId
    let tidStrPart = S.fromString $ "[" <> show tid <> "] "
    liftIO (S.hPutStrLn stderr (tidStrPart <> (fromLogStr (toLogStr msg))))

share
  [mkPersist sqlSettings, mkDeleteCascade sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
-- Maps entity to its proper form
NamedPropers
    entity T.Text
    proper T.Text
    Primary entity
|]

main :: IO ()
main = do
  let connString = "host=localhost port=5432 user=postgres dbname=persistent_transaction_weirdness password=password"
  P.withPostgresqlPool (S.fromText connString) 5 $ \pool ->
    flip P.runSqlPool pool $ P.runMigration migrateAll
  let streamOfNames = take 1000000 (cycle ["John", "Paul", "George", "Ringo"])
  P.withPostgresqlPool (S.fromText connString) 5 $ \pool -> do
    runDb pool $ P.deleteWhere ([] :: [P.Filter NamedPropers])
    UnliftIO.pooledForConcurrentlyN_ 8 streamOfNames $ \name -> do
      runDb pool $ do
        mNEProp <- P.get (NamedPropersKey name)
        case mNEProp of
          Just _neProp -> do
            pure ()
          Nothing -> do
            P.repsert
              (NamedPropersKey name)
              ( NamedPropers
                  { namedPropersEntity = name,
                    namedPropersProper = T.toLower name
                  }
              )

runDb :: Pool P.SqlBackend -> ReaderT P.SqlBackend IO b -> IO b
runDb pool f = do
  liftIO $ flip P.runSqlPool pool $ f
