{-# LANGUAGE OverloadedStrings #-}

module PlutusBenchmark.BlsField.RunBlsField
( runBlsField
) where

import PlutusBenchmark.BlsField.Scripts 
    -- scalar field
    ( addScalarsScript
    , mulScalarsScript
    , modExpScalarScriptBitShift
    , modExpScalarScriptPowMod
    , invertScalarsScriptExtEuclAlgo
    -- base field
    , addFpScript
    , mulFpScript
    , modExpFpScriptBitShift
    , modExpFpScriptPowMod
    , invertFpScriptExtEuclAlgo 
    , listOfSizedByteStrings)
import PlutusBenchmark.Common ( printHeader, printSizeStatistics, TestSize(TestSize) )

import PlutusTx.Builtins (byteStringToInteger, toBuiltin)
import PlutusTx.Prelude (modulo)
import Plutus.Crypto.BlsUtils (mkScalar, bls12_381_field_prime, bls12_381_base_prime, mkFp)

import System.IO (Handle)
import Text.Printf (hPrintf)

printCostsAddScalarScript :: Handle -> Integer -> IO ()
printCostsAddScalarScript h n =
    let f = (mkScalar . (\x -> x `modulo` bls12_381_field_prime) . byteStringToInteger . toBuiltin)
        script = addScalarsScript $ map f (listOfSizedByteStrings n 32)
    in printSizeStatistics h (TestSize n) script

printCostsMulScalarScript :: Handle -> Integer -> IO ()
printCostsMulScalarScript h n =
    let f = (mkScalar . (\x -> x `modulo` bls12_381_field_prime) . byteStringToInteger . toBuiltin)
        script = mulScalarsScript $ map f (listOfSizedByteStrings n 32)
    in printSizeStatistics h (TestSize n) script

printCostsModExpScalarBitShift :: Handle -> Integer -> IO ()
printCostsModExpScalarBitShift h n =
    let f = (mkScalar . (\x -> x `modulo` bls12_381_field_prime) . byteStringToInteger . toBuiltin)
        script = modExpScalarScriptBitShift $ map f (listOfSizedByteStrings n 32)
    in printSizeStatistics h (TestSize n) script

printCostsModExpScalarPowMod :: Handle -> Integer -> IO ()
printCostsModExpScalarPowMod h n =
    let f = (mkScalar . (\x -> x `modulo` bls12_381_field_prime) . byteStringToInteger . toBuiltin)
        script = modExpScalarScriptPowMod $ map f (listOfSizedByteStrings n 32)
    in printSizeStatistics h (TestSize n) script

printCostsInvertScalarsExtEuclAlgo :: Handle -> Integer -> IO ()
printCostsInvertScalarsExtEuclAlgo h n =
    let f = (mkScalar . (\x -> x `modulo` bls12_381_field_prime) . byteStringToInteger . toBuiltin)
        script = invertScalarsScriptExtEuclAlgo $ map f (listOfSizedByteStrings n 32)
    in printSizeStatistics h (TestSize n) script

printCostsAddFpScript :: Handle -> Integer -> IO ()
printCostsAddFpScript h n =
    let f = (mkFp . (\x -> x `modulo` bls12_381_base_prime) . byteStringToInteger . toBuiltin)
        script = addFpScript $ map f (listOfSizedByteStrings n 48)
    in printSizeStatistics h (TestSize n) script

printCostsMulFpScript :: Handle -> Integer -> IO ()
printCostsMulFpScript h n =
    let f = (mkFp . (\x -> x `modulo` bls12_381_base_prime) . byteStringToInteger . toBuiltin)
        script = mulFpScript $ map f (listOfSizedByteStrings n 48)
    in printSizeStatistics h (TestSize n) script

printCostsModExpFpBitShift :: Handle -> Integer -> IO ()
printCostsModExpFpBitShift h n =
    let f = (mkFp . (\x -> x `modulo` bls12_381_base_prime) . byteStringToInteger . toBuiltin)
        script = modExpFpScriptBitShift $ map f (listOfSizedByteStrings n 48)
    in printSizeStatistics h (TestSize n) script

printCostsModExpFpPowMod :: Handle -> Integer -> IO ()
printCostsModExpFpPowMod h n =
    let f = (mkFp . (\x -> x `modulo` bls12_381_base_prime) . byteStringToInteger . toBuiltin)
        script = modExpFpScriptPowMod $ map f (listOfSizedByteStrings n 48)
    in printSizeStatistics h (TestSize n) script

printCostsInvertFpExtEuclAlgo :: Handle -> Integer -> IO ()
printCostsInvertFpExtEuclAlgo h n =
    let f = (mkFp . (\x -> x `modulo` bls12_381_base_prime) . byteStringToInteger . toBuiltin)
        script = invertFpScriptExtEuclAlgo $ map f (listOfSizedByteStrings n 48)
    in printSizeStatistics h (TestSize n) script

runBlsField :: Handle -> IO ()
runBlsField h = do
    hPrintf h "\n\n"

    hPrintf h "n scalars field additions (size 32 bytes mod q)\n\n"
    printHeader h
    mapM_ (printCostsAddScalarScript h) [0, 10..100]
    hPrintf h "\n\n"

    hPrintf h "n scalars field multiplications (size 32 bytes mod q)\n\n"
    printHeader h
    mapM_ (printCostsMulScalarScript h) [0, 10..100]
    hPrintf h "\n\n"

    hPrintf h "n scalars field exponentiations via bitshifts (size 32 bytes mod q)\n\n"
    printHeader h
    mapM_ (printCostsModExpScalarBitShift h) [0..10]
    hPrintf h "\n\n"

    hPrintf h "n scalars field exponentiations via powMod (size 32 bytes mod q)\n\n"
    printHeader h
    mapM_ (printCostsModExpScalarPowMod h) [0..10]
    hPrintf h "\n\n"

    hPrintf h "n scalars field inversion via ext euclidean algo(size 32 bytes mod q)\n\n"
    printHeader h
    mapM_ (printCostsInvertScalarsExtEuclAlgo h) [0..10]
    hPrintf h "\n\n"

    hPrintf h "n fp field additions (size 48 bytes mod p)\n\n"
    printHeader h
    mapM_ (printCostsAddFpScript h) [0, 10..100]
    hPrintf h "\n\n"

    hPrintf h "n fp field multiplications (size 48 bytes mod p)\n\n"
    printHeader h
    mapM_ (printCostsMulFpScript h) [0, 10..100]
    hPrintf h "\n\n"

    hPrintf h "n fp field exponentiations via bitshifts (size 48 bytes mod p)\n\n"
    printHeader h
    mapM_ (printCostsModExpFpBitShift h) [0..10]
    hPrintf h "\n\n"

    hPrintf h "n fp field exponentiations via powMod (size 48 bytes mod p)\n\n"
    printHeader h
    mapM_ (printCostsModExpFpPowMod h) [0..10]
    hPrintf h "\n\n"

    hPrintf h "n base field inversion via ext euclidean algo(size 48 bytes mod p)\n\n"
    printHeader h
    mapM_ (printCostsInvertFpExtEuclAlgo h) [0..10]
    hPrintf h "\n\n"