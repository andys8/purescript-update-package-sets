module Main where

import Prelude
import Data.Array (take)
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import PackageSets (Package(..), requestPackages)
import VersionCompare (VersionComparison, check)

main :: Effect Unit
main = do
  launchAff_ printInformationFromApis

printInformationFromApis :: Aff Unit
printInformationFromApis = do
  versions <- take 20 <$> requestPackages
  result <- check versions
  for_ result $ printResult >>> log
  pure unit

printResult :: Tuple Package VersionComparison -> String
printResult (Tuple (Package { name }) result) = name <> " " <> show result
