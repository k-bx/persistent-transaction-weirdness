{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import qualified Control.Concurrent
import Control.Monad
import Control.Monad.Logger (MonadLogger (..), fromLogStr, toLogStr)
import Control.Monad.Trans.Reader
import Data.Pool
import qualified Data.String.Class as S
import qualified Data.Text as T
import qualified Database.Esqueleto as E
import qualified Database.Esqueleto.PostgreSQL as E
import qualified Database.Persist.Postgresql as P
import Database.Persist.TH
import Text.InterpolatedString.Perl6 (q)
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
    UniqueEntity entity
|]

main :: IO ()
main = do
  let connString = "host=localhost port=5432 user=postgres dbname=persistent_transaction_weirdness password=password"
  P.withPostgresqlPool (S.fromText connString) 5 $ \pool ->
    flip P.runSqlPool pool $ P.runMigration migrateAll
  let streamOfNames = take 10000 (cycle ["John", "Paul", "George", "Ringo"])
  P.withPostgresqlPool (S.fromText connString) 5 $ \pool -> do
    runDb pool $ P.deleteWhere ([] :: [P.Filter NamedPropers])
    UnliftIO.pooledForConcurrentlyN_ 8 streamOfNames $ \name -> do
      runDb pool $ do
        mNEProp <- P.get (NamedPropersKey name)
        case mNEProp of
          Just _neProp -> do
            pure ()
          Nothing -> do
            let v =
                  NamedPropers
                    { namedPropersEntity = name,
                      namedPropersProper = T.toLower name
                    }
            -- E.rawExecute
            --   [q|insert into named_propers (entity, proper) values (?, ?)
            --      on conflict do nothing|]
            --   [P.PersistText (T.toLower name), P.PersistText name]
            P.upsert v [NamedPropersProper P.=. T.toLower name]
            -- P.repsert
            --   (NamedPropersKey name)
            --   ( NamedPropers
            --       { namedPropersEntity = name,
            --         namedPropersProper = T.toLower name
            --       }
            --   )
            pure ()

runDb :: Pool P.SqlBackend -> ReaderT P.SqlBackend IO b -> IO b
runDb pool f = do
  -- liftIO $ flip P.runSqlPoolWithIsolation pool f P.Serializable
  liftIO $ flip P.runSqlPool pool $ f
