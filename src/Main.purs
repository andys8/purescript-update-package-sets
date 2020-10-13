module Main where

import Prelude
import Control.Monad.State (StateT, execStateT, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Data.Map (Map)
import Data.Map as M
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Github (GithubToken(..), requestPatchIssue)
import GithubIssue (mkIssueContent)
import Node.Process (argv, exit)
import PackageSets (Package, requestPackages)
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
  packageComparison <-
    execStateT
      (traverse_ (runPackage token) packages)
      M.empty
  requestPatchIssue token $ mkIssueContent packageComparison
  liftEffect $ exit 0

runPackage :: GithubToken -> Package -> StateT (Map Package VersionComparison) Aff Unit
runPackage token package = do
  comparison <- lift $ comparePackage token package
  log $ show package <> " " <> show comparison
  case comparison of
    VersionOkay _ -> pure unit
    VersionOutdated _ _ -> modify_ $ M.insert package comparison
    VersionComparisonFailed _ -> modify_ $ M.insert package comparison
