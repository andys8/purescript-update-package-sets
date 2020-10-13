module Main where

import Prelude
import Control.Monad.State (StateT, execStateT, modify_)
import Control.Monad.Trans.Class (lift)
import Data.Array (intercalate, take)
import Data.Map (Map)
import Data.Map as M
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Github (GithubToken(..), IssueContent, requestPatchIssue)
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
  let
    state = for_ (take 10 packages) $ runPackage token
  packageComparison <- execStateT state M.empty
  requestPatchIssue token $ mkIssueContent packageComparison
  liftEffect $ exit 0

runPackage :: GithubToken -> Package -> StateT (Map Package VersionComparison) Aff Unit
runPackage token package = do
  comparison <- lift $ comparePackage token package
  log $ show package <> " " <> show comparison
  case comparison of
    VersionOkay _ -> pure unit
    _ -> modify_ (M.insert package comparison)

mkIssueContent :: Map Package VersionComparison -> IssueContent
mkIssueContent packageComparison = { title, body, state }
  where
  state = if M.isEmpty packageComparison then "closed" else "open"

  title = "Package Updates: " <> summary

  summary =
    if M.isEmpty packageComparison then
      "All packages are up-to-date"
    else
      (show $ M.size packageComparison) <> " outdated packages"

  body = intercalate "\n\n" $ [ packages, explainer ]

  packages =
    "# Packages\n\n"
      <> summary
      <> "\n\n"
      <> intercalate "\n" packageLines

  packageLines :: Array String
  packageLines = do
    Tuple k v <- M.toUnfoldable packageComparison
    pure $ "* " <> show k <> " " <> show v

  explainer =
    "# About this Issue\n\n"
      <> "This issue is **updated automatically**. "
      <> "It can be closed and will be re-opened in case there are new updates available.\n\n"
      <> "See <https://github.com/andys8/purescript-update-package-sets>"
