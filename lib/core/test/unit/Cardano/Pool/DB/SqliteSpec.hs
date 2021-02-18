{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- DBLayer tests for SQLite implementation.

module Cardano.Pool.DB.SqliteSpec
    ( spec
    ) where

import Prelude

import Cardano.BM.Trace
    ( traceInTVarIO )
import Cardano.DB.Sqlite
    ( DBLog (..), newInMemorySqliteContext )
import Cardano.Pool.DB
    ( DBLayer (..) )
import Cardano.Pool.DB.Log
    ( PoolDbLog (..) )
import Cardano.Pool.DB.Properties
    ( properties )
import Cardano.Pool.DB.Sqlite
    ( createViews, newDBLayer, withDBLayer )
import Cardano.Pool.DB.Sqlite.TH
    ( migrateAll )
import Cardano.Wallet.DummyTarget.Primitive.Types
    ( dummyTimeInterpreter )
import Control.Tracer
    ( contramap )
import System.Directory
    ( copyFile )
import System.FilePath
    ( (</>) )
import Test.Hspec
    ( Spec, before, describe, it, shouldBe )
import Test.Hspec.Extra
    ( parallel )
import Test.Utils.Paths
    ( getTestData )
import Test.Utils.Trace
    ( captureLogging )
import UnliftIO.STM
    ( TVar, newTVarIO )
import UnliftIO.Temporary
    ( withSystemTempDirectory )

-- | Set up a DBLayer for testing, with the command context, and the logging
-- variable.
newMemoryDBLayer :: IO (DBLayer IO)
newMemoryDBLayer = snd <$> newMemoryDBLayer'

newMemoryDBLayer' :: IO (TVar [PoolDbLog], DBLayer IO)
newMemoryDBLayer' = do
    logVar <- newTVarIO []
    let tr  = traceInTVarIO logVar
    let tr' = contramap MsgGeneric tr
    ctx <- newInMemorySqliteContext tr' createViews migrateAll
    return (logVar, newDBLayer tr ti ctx)
  where
    ti = dummyTimeInterpreter

spec :: Spec
spec = parallel $ do
    before newMemoryDBLayer $ do
        parallel $ describe "Sqlite" properties

    describe "Migration Regressions" $ do
        test_migrationFromv20191216

test_migrationFromv20191216 :: Spec
test_migrationFromv20191216 =
    it "'migrate' an existing database from v2019-12-16 by\
       \ creating it from scratch again. But only once." $ do
        let orig = $(getTestData) </> "stake-pools-db" </> "v2019-12-16.sqlite"
        withSystemTempDirectory "stake-pools-db" $ \dir -> do
            let path = dir </> "stake-pools.sqlite"
            copyFile orig path
            let ti = dummyTimeInterpreter
            (logs, _) <- captureLogging $ \tr -> do
                withDBLayer tr (Just path) ti $ \_ -> pure ()
                withDBLayer tr (Just path) ti $ \_ -> pure ()

            let databaseConnMsg  = filter isMsgWillOpenDB logs
            let databaseResetMsg = filter (== MsgGeneric MsgDatabaseReset) logs
            let migrationErrMsg  = filter isMsgMigrationError logs

            length databaseConnMsg  `shouldBe` 3
            length databaseResetMsg `shouldBe` 1
            length migrationErrMsg  `shouldBe` 1

isMsgWillOpenDB :: PoolDbLog -> Bool
isMsgWillOpenDB (MsgGeneric (MsgWillOpenDB _)) = True
isMsgWillOpenDB _ = False

isMsgMigrationError :: PoolDbLog -> Bool
isMsgMigrationError (MsgGeneric (MsgMigrations (Left _))) = True
isMsgMigrationError _ = False
