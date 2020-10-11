module Github where

import Prelude
import Affjax (URL)
import Data.Either (Either, either)
import Effect.Aff (Aff)
import Simple.Ajax (AjaxError, get)

type Tag
  = { name :: String }

type TagName
  = String

type Repository
  = { repoUser :: String
    , repoName :: String
    }

requestTags :: Repository -> Aff (Array TagName)
requestTags repository = do
  resp <- get $ tagUrlApiUrl repository :: Aff (Either AjaxError (Array Tag))
  pure $ either (const []) ((<$>) _.name) resp

tagUrlApiUrl :: Repository -> URL
tagUrlApiUrl { repoUser, repoName } = baseURL <> path
  where
  baseURL = "https://api.github.com"

  path = "/repos/" <> repoUser <> "/" <> repoName <> "/tags"
