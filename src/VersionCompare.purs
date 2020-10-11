module VersionCompare (check, VersionComparison) where

import Prelude
import Data.Array (last, mapMaybe, sort)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Github (requestTags)
import PackageSets (Package(..))
import Version (Version, parseVersion)

data VersionComparison
  = VersionOkay Version
  | VersionOutdated Version Version
  | VersionComparisonFailed String

instance showVersionComparison :: Show VersionComparison where
  show (VersionOkay v) = "✔ " <> show v
  show (VersionOutdated v1 v2) = "✖ " <> show v1 <> " -> " <> show v2
  show (VersionComparisonFailed err) = "[ERROR] " <> err

check :: Array Package -> Aff (Array (Tuple Package VersionComparison))
check packages = traverse f packages
  where
  f package = do
    result <- comparePackage package
    pure $ Tuple package result

comparePackage :: Package -> Aff VersionComparison
comparePackage package@(Package { repoUser, repoName }) = do
  tags <- requestTags { repoUser, repoName }
  let
    versions = mapMaybe (hush <<< parseVersion) tags
  pure $ compareVersions package versions

compareVersions :: Package -> Array Version -> VersionComparison
compareVersions (Package { version: pkgVersion }) allVersions = case last $ sort allVersions of
  Just latestVersion
    | latestVersion == pkgVersion -> VersionOkay pkgVersion
  Just latestVersion
    | latestVersion > pkgVersion -> VersionOutdated pkgVersion latestVersion
  Just latestVersion
    | latestVersion < pkgVersion ->
      VersionComparisonFailed
        $ "Actual version "
        <> show pkgVersion
        <> " is newer than on github "
        <> show latestVersion
  Just _ -> VersionComparisonFailed $ "Recent version not found on github"
  Nothing -> VersionComparisonFailed "No version tags found"
