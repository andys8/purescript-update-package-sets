module GithubIssue where

import Prelude
import Data.Array (intercalate)
import Data.Map (Map)
import Data.Map as M
import Data.Tuple (Tuple(..))
import Github (IssueContent)
import PackageSets (Package)
import VersionCompare (VersionComparison)

mkIssueContent :: Map Package VersionComparison -> IssueContent
mkIssueContent packageComparison = { title, body, state }
  where
  state = if M.isEmpty packageComparison then "closed" else "open"

  title = "Version Check: " <> summary

  summary =
    if M.isEmpty packageComparison then
      "All packages up-to-date ✔️"
    else
      (show $ M.size packageComparison) <> " packages affected ⚠️"

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
