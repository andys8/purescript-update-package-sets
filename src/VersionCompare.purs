module VersionCompare (runComparison, comparePackage, VersionComparison(..)) where

import Prelude
import Data.Array (last, mapMaybe, sort)
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Github (GithubToken, requestTags)
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

runComparison :: GithubToken -> Array Package -> Aff (Array (Tuple Package VersionComparison))
runComparison token packages = traverse f packages
  where
  f package = do
    result <- comparePackage token package
    pure $ Tuple package result

comparePackage :: GithubToken -> Package -> Aff VersionComparison
comparePackage token package@(Package { repoUser, repoName }) = do
  tags <- requestTags token { repoUser, repoName }
  let
    versions = mapMaybe (hush <<< parseVersion) tags
  pure $ compareVersions package versions

compareVersions :: Package -> Array Version -> VersionComparison
compareVersions (Package package) allVersions =
  let
    { version: pkgVersion, repoUser, repoName } = package
  in
    case last $ sort allVersions of
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
      Nothing -> VersionComparisonFailed $ "No version tags found for " <> repoUser <> "/" <> repoName
