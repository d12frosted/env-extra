-- | Test specs for 'env-extra'.

--------------------------------------------------------------------------------

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------

module Main where

--------------------------------------------------------------------------------

import           System.Environment.Extra

--------------------------------------------------------------------------------

import           Prelude
import           Test.Tasty
import           Test.Tasty.HUnit

--------------------------------------------------------------------------------

main :: IO ()
main
  = defaultMain
  -- don't execute setEnv in parallel
  $ sequentialTestGroup "env extra" AllFinish
  [ testCase "return Nothing when variable is not set" $
    envMaybe "DEFINITELY_NOT_SET" >>= (@?= Nothing)

  , testCase "return value when variable is set" $
    setEnv "NAME" "Boris" >> envMaybe "NAME" >>= (@?= Just "Boris")

  , testCase "read decimal values" $
    setEnv "AGE" "12" >> envRead decimal "AGE" >>= (@?= Just 12)

  , testCase "return Nothing when reading non-decimal value as decimal" $
    setEnv "AGE" "malformed" >> envRead decimal "AGE" >>= (@?= Nothing)
  ]

--------------------------------------------------------------------------------
