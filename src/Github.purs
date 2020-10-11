module Github where

import Prelude
import Affjax (URL)
import Affjax.RequestHeader (RequestHeader(..))
import Data.Either (Either, either)
import Effect.Aff (Aff)
import Partial.Unsafe (unsafeCrashWith)
import Simple.Ajax (AjaxError, getR)

type Tag
  = { name :: String }

type TagName
  = String

type Repository
  = { repoUser :: String
    , repoName :: String
    }

newtype GithubToken
  = GithubToken String

requestTags :: GithubToken -> Repository -> Aff (Array TagName)
requestTags (GithubToken token) repository = do
  resp <- getR { headers } $ tagUrlApiUrl repository :: Aff (Either AjaxError (Array Tag))
  pure $ either (unsafeCrashWith <<< show) ((<$>) _.name) resp
  where
  headers = [ RequestHeader "Authorization" ("Bearer " <> token) ]

tagUrlApiUrl :: Repository -> URL
tagUrlApiUrl { repoUser, repoName } = baseURL <> path
  where
  baseURL = "https://api.github.com"

  path = "/repos/" <> repoUser <> "/" <> repoName <> "/tags"
