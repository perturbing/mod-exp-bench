{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE Strict                #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -O0                #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusBenchmark.BlsField.Scripts
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
, listOfSizedByteStrings
) where

import PlutusTx (compile, unsafeApplyCode, liftCodeDef, getPlcNoAnn)
import PlutusTx.Prelude 
    ( map
    , Integer
    , foldr
    , ($)
    , (.)
    , (*)
    , (+))
import PlutusTx.Numeric (zero, one)
import PlutusTx.Builtins (integerToByteString)

import PlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC

import Prelude qualified as Haskell
import Hedgehog.Internal.Gen qualified as G
import Hedgehog.Internal.Range qualified as R
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString)

import Plutus.Crypto.BlsUtils 
    ( Scalar
    , mkScalar
    , unScalar
    , Fp
    , mkFp
    , unFp
    , bls12_381_field_prime
    , bls12_381_base_prime
    , multiplicativeInverse
    , modularExponentiationScalar
    , modularExponentiationFp
    , powMod
    , powModFp
    )

{-# NOINLINE listOfSizedByteStrings #-}
listOfSizedByteStrings :: Integer -> Integer -> [ByteString]
listOfSizedByteStrings n l = unsafePerformIO . G.sample $
                             G.list (R.singleton $ Haskell.fromIntegral n)
                                  (G.bytes (R.singleton $ Haskell.fromIntegral l))

-- Scalar field scripts

{-# INLINABLE addScalars #-}
addScalars :: [Scalar] -> Scalar
addScalars = foldr (+) zero

addScalarsScript :: [Scalar] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
addScalarsScript xs =
    getPlcNoAnn $ $$(compile [|| addScalars ||])
       `unsafeApplyCode` liftCodeDef xs

{-# INLINABLE mulScalars #-}
mulScalars :: [Scalar] -> Scalar
mulScalars = foldr (*) one

mulScalarsScript :: [Scalar] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mulScalarsScript xs =
    getPlcNoAnn $ $$(compile [|| mulScalars ||])
       `unsafeApplyCode` liftCodeDef xs

{-# INLINABLE modExpScalarBitShift #-}
modExpScalarBitShift :: [Scalar] -> [Scalar]
modExpScalarBitShift = map (`modularExponentiationScalar` (integerToByteString 52435875175126190479447740508185965837690552500527637822603658699938581184511))

modExpScalarScriptBitShift :: [Scalar] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
modExpScalarScriptBitShift xs =
    getPlcNoAnn $ $$(compile [|| modExpScalarBitShift ||])
       `unsafeApplyCode` liftCodeDef xs

{-# INLINABLE modExpScalarPowMod #-}
modExpScalarPowMod :: [Scalar] -> [Scalar]
modExpScalarPowMod = map (`powMod` 52435875175126190479447740508185965837690552500527637822603658699938581184511)

modExpScalarScriptPowMod :: [Scalar] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
modExpScalarScriptPowMod xs =
    getPlcNoAnn $ $$(compile [|| modExpScalarPowMod ||])
       `unsafeApplyCode` liftCodeDef xs

{-# INLINABLE invertScalarsExtEuclAlgo #-}
invertScalarsExtEuclAlgo :: [Scalar] -> [Scalar]
invertScalarsExtEuclAlgo = map (mkScalar . multiplicativeInverse bls12_381_field_prime . unScalar)

invertScalarsScriptExtEuclAlgo :: [Scalar] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
invertScalarsScriptExtEuclAlgo xs =
    getPlcNoAnn $ $$(compile [|| invertScalarsExtEuclAlgo ||])
       `unsafeApplyCode` liftCodeDef xs

-- base field scripts

{-# INLINABLE addFp #-}
addFp :: [Fp] -> Fp
addFp = foldr (+) zero

addFpScript :: [Fp] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
addFpScript xs =
    getPlcNoAnn $ $$(compile [|| addFp ||])
       `unsafeApplyCode` liftCodeDef xs

{-# INLINABLE mulFp #-}
mulFp :: [Fp] -> Fp
mulFp = foldr (*) one

mulFpScript :: [Fp] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
mulFpScript xs =
    getPlcNoAnn $ $$(compile [|| mulFp ||])
       `unsafeApplyCode` liftCodeDef xs

{-# INLINABLE modExpFpBitShift #-}
modExpFpBitShift :: [Fp] -> [Fp]
modExpFpBitShift = map (`modularExponentiationFp` (integerToByteString 52435875175126190479447740508185965837690552500527637822603658699938581184511))

modExpFpScriptBitShift :: [Fp] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
modExpFpScriptBitShift xs =
    getPlcNoAnn $ $$(compile [|| modExpFpBitShift ||])
       `unsafeApplyCode` liftCodeDef xs

{-# INLINABLE modExpFpPowMod #-}
modExpFpPowMod :: [Fp] -> [Fp]
modExpFpPowMod = map (`powModFp` 52435875175126190479447740508185965837690552500527637822603658699938581184511)

modExpFpScriptPowMod :: [Fp] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
modExpFpScriptPowMod xs =
    getPlcNoAnn $ $$(compile [|| modExpFpPowMod ||])
       `unsafeApplyCode` liftCodeDef xs

{-# INLINABLE invertFpExtEuclAlgo #-}
invertFpExtEuclAlgo :: [Fp] -> [Fp]
invertFpExtEuclAlgo = map (mkFp . multiplicativeInverse bls12_381_base_prime . unFp)

invertFpScriptExtEuclAlgo :: [Fp] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
invertFpScriptExtEuclAlgo xs =
    getPlcNoAnn $ $$(compile [|| invertFpExtEuclAlgo ||])
       `unsafeApplyCode` liftCodeDef xs

