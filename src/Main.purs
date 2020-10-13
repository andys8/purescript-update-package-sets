module Main where

import Prelude
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Github (GithubToken(..), requestPatchIssue)
import Node.Process (argv, exit)
import PackageSets (requestPackages)
import VersionCompare (VersionComparison(..), comparePackage)

main :: Effect Unit
main = do
  arguments <- argv
  case arguments of
    [ _, _, token ]
      | token /= "" -> launchAff_ $ run (GithubToken token)
    _ -> log $ "Usage: npm start <github-token>"

run :: GithubToken -> Aff Unit
run token = do
  packages <- requestPackages
  -- for_ packages
  --   $ \package -> do
  --       comparison <- comparePackage token package
  --       case comparison of
  --         VersionOkay _ -> pure unit
  --         _ -> log $ show package <> " " <> show comparison
  requestPatchIssue token
    { title: "Test creating an issue with api"
    , body: "TestBody\nContent"
    }
  liftEffect $ exit 0
