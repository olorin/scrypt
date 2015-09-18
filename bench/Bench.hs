{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Control.DeepSeq

import           Criterion.Main

import           Crypto.Scrypt

import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Monoid

import           GHC.Generics (Generic)

import           Test.QuickCheck
import           Test.QuickCheck.Instances()

deriving instance Generic EncryptedPass

instance NFData EncryptedPass

testHashes :: Int
           -> Int
           -> Gen ((ByteString, ByteString), (EncryptedPass, EncryptedPass))
testHashes small big = do
    short1 <- vectorOf small arbitrary
    short2 <- vectorOf small arbitrary `suchThat` (/= short1)
    long <- vectorOf big arbitrary
    let b1 = BS.pack $ short1 <> long
    let b2 = BS.pack $ short2 <> long
    return ((b1, b2), (EncryptedPass b1, EncryptedPass b2))

main :: IO ()
main = defaultMain [
    env (generate $ testHashes 2 128) $ \ ~((bs1, bs2), (h1, h2)) ->
        bgroup "Eq" $
            [ bench "ByteString/same" $ nf (uncurry (==)) (bs1, bs1)
            , bench "ByteString/different" $ nf (uncurry (==)) (bs1, bs2)
            , bench "EncryptedPass/same" $ nf (uncurry (==)) (h1, h1)
            , bench "EncryptedPass/different" $ nf (uncurry (==)) (h1, h2)
            ]
    ]
