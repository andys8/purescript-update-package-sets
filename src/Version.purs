module Version where

import Prelude
import Data.Array (catMaybes, intercalate, mapMaybe)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)

data Version
  = Version
    { major :: Int
    , minor :: Int
    , patch :: Int
    }

instance showVersion :: Show Version where
  show (Version { major, minor, patch }) = "v" <> versionString
    where
    versionString = intercalate "." $ show <$> [ major, minor, patch ]

derive instance eqVersion :: Eq Version

derive instance ordVersion :: Ord Version

parseVersion :: String -> Either String Version
parseVersion str = do
  versionRegex <- regex """^v(\d+)\.(\d+)\.(\d+)$""" noFlags
  mkVersion $ match versionRegex str
  where
  mkVersion Nothing = Left $ "Version '" <> str <> "' not match format 'v0.0.0'"

  mkVersion (Just xs) = toVersion $ mapMaybe fromString $ catMaybes $ toArray xs

  toVersion [ major, minor, patch ] = Right $ Version { major, minor, patch }

  toVersion x = Left $ "Version '" <> str <> "' does not have exactly major, minor, patch" <> show x
