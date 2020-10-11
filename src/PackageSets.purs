module PackageSets where

import Prelude
import Affjax (URL)
import Data.Either (Either(..), either, note)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), split, stripPrefix, stripSuffix)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Foreign.Object (Object, toUnfoldable)
import Partial.Unsafe (unsafeCrashWith)
import Simple.Ajax (get)
import Version (Version, parseVersion)

urlPackagesJson :: URL
urlPackagesJson = "https://raw.githubusercontent.com/purescript/package-sets/master/packages.json"

type Response
  = Object RespPackage

type RespPackage
  = { repo :: String
    , version :: String
    , dependencies :: Array String
    }

data Package
  = Package
    { name :: String
    , repoName :: String
    , repoUser :: String
    , version :: Version
    , dependencies :: Array String
    }

instance showPackage :: Show Package where
  show (Package { name, repoUser, repoName }) = name <> " (" <> repoUser <> "/" <> repoName <> ")"

requestPackages :: Aff (Array Package)
requestPackages = do
  resp <- get urlPackagesJson
  pure
    $ either (const []) (map mkPackage <<< toUnfoldable) resp

mkPackage :: Tuple String RespPackage -> Package
mkPackage (Tuple name { repo, version: versionString, dependencies }) =
  either unsafeCrashWith identity
    $ do
        version <- parseVersion versionString
        { repoUser, repoName } <- parseGithubRepo repo
        pure $ Package { name, dependencies, repoName, repoUser, version }

-- TODO: Clean up variable names
parseGithubRepo :: String -> Either String { repoUser :: String, repoName :: String }
parseGithubRepo str = do
  let
    y = fromMaybe str $ stripSuffix (Pattern "/") str

    x = fromMaybe y $ stripSuffix (Pattern ".git") y
  z <- note "Not a github repository" $ stripPrefix (Pattern "https://github.com/") x
  case split (Pattern "/") z of
    [ repoUser, repoName ] -> Right { repoUser, repoName }
    _ -> Left $ "Couldn't extract user and repo of url " <> str
