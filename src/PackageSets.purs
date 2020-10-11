module PackageSets where

import Prelude
import Affjax (URL)
import Data.Either (either)
import Effect.Aff (Aff)
import Foreign.Object (Object, values)
import Simple.Ajax (get)

urlPackagesJson :: URL
urlPackagesJson = "https://raw.githubusercontent.com/purescript/package-sets/master/packages.json"

type Response
  = Object Package

type Package
  = { repo :: String
    , version :: String
    , dependencies :: Array String
    }

requestPackages :: Aff (Array Package)
requestPackages = do
  resp <- get urlPackagesJson
  pure $ either (const []) values resp

-- newtype Version
--   = Version String
-- instance readForeignVersion :: ReadForeign Version where
--   readImpl v = Version <$> readImpl v
-- instance showVersion :: Show Version where
--   show (Version v) = v
