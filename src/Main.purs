module Main where

import Prelude
import Data.Array (take)
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log, logShow)
import Github (requestTags)
import PackageSets (requestPackages)

main :: Effect Unit
main = do
  log "🍝"
  launchAff_ printInformationFromApis

printInformationFromApis :: Aff Unit
printInformationFromApis = do
  versions <- take 5 <$> requestPackages
  for_ versions $ logShow
  tags <- requestTags
  logShow tags
  pure unit
