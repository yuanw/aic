{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main
import Aic

prop_test :: Property
prop_test = property $ do
  doAic === "AIC"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
