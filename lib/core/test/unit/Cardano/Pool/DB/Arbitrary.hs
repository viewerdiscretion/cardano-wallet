{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Pool.DB.Arbitrary
    ( StakePoolsFixture (..)
    , StakePoolOwnersFixture (..)
    ) where

import Prelude

import Cardano.Wallet.DummyTarget.Primitive.Types
    ( genesisParameters )
import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..)
    , EpochLength (..)
    , EpochNo (..)
    , Hash (..)
    , PoolId (..)
    , PoolOwner (..)
    , SlotId (..)
    , SlotNo (..)
    , SlotParameters (..)
    , slotSucc
    , unsafeEpochNo
    )
import Control.Arrow
    ( second )
import Control.Monad
    ( foldM )
import Data.Generics.Internal.VL.Lens
    ( (^.) )
import Data.Ord
    ( Down (..) )
import Data.Quantity
    ( Quantity (..) )
import Data.Word
    ( Word32, Word64 )
import Data.Word.Odd
    ( Word31 )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , arbitrarySizedBoundedIntegral
    , choose
    , elements
    , listOf
    , shrinkIntegral
    , shrinkList
    , shuffle
    , vectorOf
    )

import qualified Data.ByteString.Char8 as B8
import qualified Data.List as L

{-------------------------------------------------------------------------------
                                 Modifiers
-------------------------------------------------------------------------------}

data StakePoolsFixture = StakePoolsFixture
    { poolSlots :: [(PoolId, BlockHeader)]
    , rollbackSlots :: [SlotId] }
    deriving stock (Eq, Show)

data StakePoolOwnersFixture = StakePoolOwnersFixture
    { fixturePoolId :: PoolId
    , fixturePoolOwners :: [(PoolId, PoolOwner)] }
    deriving stock (Eq, Show)

{-------------------------------------------------------------------------------
                                 Stake Pools
-------------------------------------------------------------------------------}

instance Arbitrary SlotId where
    shrink (SlotId ep sl) =
        uncurry SlotId <$> shrink (ep, sl)
    arbitrary = SlotId <$> arbitrary <*> arbitrary

instance Arbitrary SlotNo where
    shrink (SlotNo x) = SlotNo <$> shrink x
    arbitrary = SlotNo <$> choose (0, fromIntegral arbitraryChainLength)

instance Arbitrary EpochNo where
    shrink (EpochNo x) = EpochNo <$> shrink x
    arbitrary = unsafeEpochNo <$> choose (0, arbitraryEpochLength)

instance Arbitrary Word31 where
    arbitrary = arbitrarySizedBoundedIntegral
    shrink = shrinkIntegral

instance Arbitrary (Quantity "lovelace" Word64) where
    shrink (Quantity q) = [ Quantity q' | q' <- shrink q ]
    arbitrary = Quantity <$> arbitrary

arbitraryEpochLength :: Word32
arbitraryEpochLength = 100

arbitraryChainLength :: Word32
arbitraryChainLength = 10

instance Arbitrary PoolId where
    arbitrary = do
        bytes <- vectorOf 32 (elements ['a'..'z'])
        return $ PoolId $ B8.pack bytes

instance Arbitrary StakePoolOwnersFixture where
    arbitrary = StakePoolOwnersFixture <$> fmap PoolId genBytes <*> listOf entry
      where
        entry = (,) <$> fmap PoolId genBytes <*> fmap PoolOwner genBytes
        genBytes = B8.replicate 32 <$> elements ['a'..'e']
    shrink (StakePoolOwnersFixture p xs) =
        StakePoolOwnersFixture p <$> shrinkList (const []) xs

instance Arbitrary StakePoolsFixture where
    arbitrary = do
        poolsNumber <- choose (1, 100)
        pools <- vectorOf poolsNumber arbitrary
        slotsNumber <- choose (0, 200)
        firstSlot <- arbitrary
        slotsGenerated <-
            foldM (appendPair pools) [] (generateNextSlots [firstSlot] slotsNumber)
        rNum <- choose (1, slotsNumber + 1)
        rSlots <-
            (L.sortOn Down . take rNum) <$> shuffle (map snd slotsGenerated)
        pure $ StakePoolsFixture (second mkBlockHeader <$> slotsGenerated) rSlots
      where
        mkBlockHeader :: SlotId -> BlockHeader
        mkBlockHeader s = BlockHeader
            { slotId = s
            , blockHeight = Quantity 0
            , headerHash = Hash "00000000000000000000000000000001"
            , parentHeaderHash = Hash "00000000000000000000000000000000"
            }

        epochLength :: EpochLength
        epochLength = genesisParameters ^. #getEpochLength

        sp :: SlotParameters
        sp = SlotParameters
            epochLength
            (genesisParameters ^. #getSlotLength)
            (genesisParameters ^. #getGenesisBlockDate)

        generateNextSlots :: [SlotId] -> Int -> [SlotId]
        generateNextSlots slots@(s:_) num =
            if (num < 1) then
                reverse slots
            else
                generateNextSlots ((slotSucc sp s):slots) (num - 1)
        generateNextSlots [] _ = []

        appendPair
            :: [PoolId]
            -> [(PoolId, SlotId)]
            -> SlotId
            -> Gen [(PoolId, SlotId)]
        appendPair pools pairs slot = do
            pool <- elements pools
            return $ (pool,slot):pairs