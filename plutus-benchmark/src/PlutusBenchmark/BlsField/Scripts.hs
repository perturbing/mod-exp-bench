{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE Strict                #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}

module PlutusBenchmark.BlsField.Scripts
( invertScalarsScriptExtEuclAlgo
, listOfSizedByteStrings
) where

import PlutusTx (compile, unsafeApplyCode, liftCodeDef, getPlcNoAnn)
import PlutusTx.Prelude (map, Integer, foldr, ($), (.), divide, modulo)

import PlutusCore (DefaultFun, DefaultUni)
import UntypedPlutusCore qualified as UPLC

import Prelude qualified as Haskell
import Hedgehog.Internal.Gen qualified as G
import Hedgehog.Internal.Range qualified as R
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString)

import Plutus.Crypto.BlsUtils (Scalar, mkScalar, unScalar, bls12_381_field_prime, multiplicativeInverse)

{-# NOINLINE listOfSizedByteStrings #-}
listOfSizedByteStrings :: Integer -> Integer -> [ByteString]
listOfSizedByteStrings n l = unsafePerformIO . G.sample $
                             G.list (R.singleton $ Haskell.fromIntegral n)
                                  (G.bytes (R.singleton $ Haskell.fromIntegral l))

{-# INLINABLE invertScalarsExtEuclAlgo #-}
invertScalarsExtEuclAlgo :: [Scalar] -> [Scalar]
invertScalarsExtEuclAlgo = map (mkScalar . multiplicativeInverse bls12_381_field_prime . unScalar)

invertScalarsScriptExtEuclAlgo :: [Scalar] -> UPLC.Program UPLC.NamedDeBruijn DefaultUni DefaultFun ()
invertScalarsScriptExtEuclAlgo xs =
    getPlcNoAnn $ $$(compile [|| invertScalarsExtEuclAlgo ||])
       `unsafeApplyCode` liftCodeDef xs

