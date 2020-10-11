module Version (parseVersion, Version) where

import Prelude
import Data.Array (catMaybes, intercalate, mapMaybe)
import Data.Array.NonEmpty (toArray)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.Regex (match, regex)
import Data.String.Regex.Flags (noFlags)

data Version
  = Version
    { major :: Int
    , minor :: Int
    , patch :: Int
    , suffix :: Maybe Suffix
    }

type Suffix
  = String

instance showVersion :: Show Version where
  show (Version { major, minor, patch }) = "v" <> versionString
    where
    versionString = intercalate "." $ show <$> [ major, minor, patch ]

derive instance eqVersion :: Eq Version

derive instance ordVersion :: Ord Version

parseVersion :: String -> Either String Version
parseVersion str = do
  case split (Pattern "-") str of
    [ v ] -> parseSemanticVersion v
    [ v, "" ] -> Left $ "Unexpected empty suffix"
    [ v, s ] -> do
      Version { major, minor, patch } <- parseSemanticVersion v
      pure $ Version { major, minor, patch, suffix: Just s }
    x -> Left $ "Version '" <> str <> "' does not match format 'v0.0.0[-suffix]' " <> show x

parseSemanticVersion :: String -> Either String Version
parseSemanticVersion str = do
  versionRegex <- regex """^v(\d+)\.(\d+)\.(\d+)$""" noFlags
  mkVersion $ match versionRegex str
  where
  mkVersion Nothing = Left $ "Version '" <> str <> "' does not match format 'v0.0.0'"

  mkVersion (Just xs) = toVersion $ mapMaybe fromString $ catMaybes $ toArray xs

  toVersion [ major, minor, patch ] = Right $ Version { major, minor, patch, suffix: Nothing }

  toVersion x = Left $ "Version '" <> str <> "' does not have exactly major, minor, patch" <> show x
