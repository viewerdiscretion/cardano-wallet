{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Hspec.ExtraSpec where

import Prelude

import Control.Monad.IO.Unlift
    ( MonadUnliftIO (..) )
import Data.IORef
    ( IORef, newIORef, readIORef, writeIORef )
import Data.List
    ( isPrefixOf )
import System.Environment
    ( setEnv )
import System.IO.Silently
    ( capture_ )
import Test.Hspec
    ( ActionWith
    , Expectation
    , Spec
    , SpecWith
    , beforeAll
    , before_
    , describe
    , expectationFailure
    , it
    , shouldBe
    , shouldContain
    )
import Test.Hspec.Core.Runner
    ( defaultConfig, runSpec )
import Test.Hspec.Core.Spec
    ( runIO, sequential )
import Test.Hspec.Expectations.Lifted
    ( shouldReturn )
import Test.Hspec.Extra
    ( aroundAll )
import UnliftIO.Concurrent
    ( threadDelay )
import UnliftIO.Exception
    ( bracket )
import UnliftIO.MVar
    ( MVar, newEmptyMVar, newMVar, putMVar, tryReadMVar, tryTakeMVar )

import qualified Test.Hspec.Extra as Extra

spec :: Spec
spec = do
    itSpec
    aroundAllSpec

itSpec :: Spec
itSpec = describe "Extra.it" $ before_ (setEnv "TESTS_RETRY_FAILED" "y") $ do
    it "equals Hspec.it on success" $ do
        let test = 1 `shouldBe` (1::Int)
        test `shouldMatchHSpecIt` test

    it "equals Hspec.it on failure" $ do
        let test = (2+2) `shouldBe` (5::Int)
        test `shouldMatchHSpecIt` test

    describe "when first attempt fails due to flakiness" $ do
        describe "when the retry succeeds" $ do
            let flaky = expectationFailure "flaky test"
            let succeed = 1 `shouldBe` (1 :: Int)
            it "succeeds" $ do
                outcomes <- newIORef [flaky, succeed]
                (dynamically outcomes) `shouldMatchHSpecIt` succeed

        describe "when the retry also fails" $ do
            -- Some tests use limited resources and cannot be retried.
            -- On failures, we should make sure to show the first failure
            -- which is the interesting one.
            it "fails with the first error" $ do
                let failure = expectationFailure "failure"
                let noRetry = expectationFailure "test can't be retried"
                outcomes <- newIORef [failure, noRetry]
                (dynamically outcomes) `shouldMatchHSpecIt` failure
    it "can time out" $ do
        let micro = (1000*1000 *)
        let timeout = do
                threadDelay (micro 10)
                expectationFailure "should have timed out"
        res <- run (Extra.itWithCustomTimeout 2) timeout
        res `shouldContain` "timed out in 2 seconds"

  where
    -- | lhs `shouldMatchHSpecIt` rhs asserts that the output of running
    -- (Extra.it "" lhs) and (Hspec.it "" rhs) are equal. Modulo random seed-
    -- and execution time-information.
    shouldMatchHSpecIt :: IO () -> IO () -> Expectation
    shouldMatchHSpecIt extraTest hspecTest = do
        extraRes <- run Extra.it extraTest
        hspecRes <- run it hspecTest
        extraRes `shouldBe` hspecRes

    run
        :: (String -> ActionWith () -> SpecWith ()) -- ^ it version
        -> IO () -- ^ test body
        -> IO String -- ^ hspec output
    run anyIt prop = fmap stripTime
        $ capture_
        $ flip runSpec defaultConfig
        $ beforeAll (return ())
        $ anyIt "<test spec>" (const prop)
      where
        -- | Remove time and seed such that we can compare the captured stdout
        -- of two different hspec runs.
        stripTime :: String -> String
        stripTime = unlines
                . filter (not . ("Finished in" `isPrefixOf`))
                . filter (not . ("Randomized" `isPrefixOf`))
                . filter (not . ("retry:" `isPrefixOf`))
                . lines

    -- | Returns an IO action that is different every time you run it!,
    -- according to the supplied IORef of outcomes.
    dynamically
        :: IORef [IO ()]
        -> IO ()
    dynamically outcomes = do
        outcome:rest <- readIORef outcomes
        writeIORef outcomes rest
        outcome

aroundAllSpec :: Spec
aroundAllSpec = sequential $ do
    let withMockResource :: MonadUnliftIO m => a -> (a -> m r) -> m r
        withMockResource a = bracket (pure a) (const $ pure ())

        withMVarResource :: (Show a, Eq a, MonadUnliftIO m) => a -> (MVar a -> m r) -> m r
        withMVarResource a = bracket (newMVar a) (takeMVarCheck a)

        takeMVarCheck :: (Show a, Eq a, MonadUnliftIO m) => a -> MVar a -> m ()
        takeMVarCheck a var = tryTakeMVar var `shouldReturn` Just a

        resourceA = 1 :: Int

    describe "Extra.aroundAll" $ do
        describe "trivial" $ aroundAll (withMockResource resourceA) $ do
            it "provides resource to first test"
                (`shouldBe` resourceA)
            it "provides resource to second test"
                (`shouldBe` resourceA)

        describe "basic" $ aroundAll (withMVarResource resourceA) $ do
            it "provides resource to first test" $ \var ->
               tryReadMVar @IO var `shouldReturn` Just resourceA

            it "provides resource to second test" $ \var ->
                tryReadMVar @IO var `shouldReturn` Just resourceA

        -- This could work with an adjustment of 'aroundAll' type
        -- describe "multi" $
        --     aroundAll (withMVarResource resourceA) $
        --     aroundAll (withMVarResource resourceB) $ do
        --        it "provides two resources" $ \a b -> do
        --            tryTakeMVar a `shouldReturn` Just resourceA
        --            tryTakeMVar b `shouldReturn` Just resourceB

        mvar <- runIO newEmptyMVar
        let withResource = bracket (putMVar mvar ()) (`takeMVarCheck` mvar)

        describe "lazy allocation" $ aroundAll withResource $ do
            before <- runIO $ tryReadMVar mvar
            it "not before the spec runs" $ \_ -> do
                before `shouldBe` Nothing
                tryReadMVar mvar `shouldReturn` Just ()

        describe "prompt release" $
            it "after the spec runs" $
                tryReadMVar @IO mvar `shouldReturn` Nothing
