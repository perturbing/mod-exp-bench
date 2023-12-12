{-# LANGUAGE OverloadedStrings #-}

module PlutusBenchmark.BlsField.RunBlsField
( runBlsField
) where

import PlutusBenchmark.BlsField.Scripts (invertScalarsScriptExtEuclAlgo, listOfSizedByteStrings)
import PlutusBenchmark.Common ( printHeader, printSizeStatistics, TestSize(TestSize) )

import PlutusTx.Builtins (byteStringToInteger, toBuiltin)
import PlutusTx.Prelude (modulo)
import Plutus.Crypto.BlsUtils (mkScalar, bls12_381_field_prime)

import System.IO (Handle)
import Text.Printf (hPrintf)

printCostsInvertScalarsExtEuclAlgo :: Handle -> Integer -> IO ()
printCostsInvertScalarsExtEuclAlgo h n =
    let f = (mkScalar . (\x -> x `modulo` bls12_381_field_prime) . byteStringToInteger . toBuiltin)
        script = invertScalarsScriptExtEuclAlgo $ map f (listOfSizedByteStrings n 32)
    in printSizeStatistics h (TestSize n) script

runBlsField :: Handle -> IO ()
runBlsField h = do
    hPrintf h "\n\n"

    hPrintf h "n scalars inversion via ext euclidean algo(size 32 bytes mod p)\n\n"
    printHeader h
    mapM_ (printCostsInvertScalarsExtEuclAlgo h) [0..10]
    hPrintf h "\n\n"