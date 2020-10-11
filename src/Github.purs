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

requestTags :: Aff (Array TagName)
requestTags = do
  let
    repository = { user: "andys8", repo: "git-brunch" }
  resp <- get $ tagUrlApiUrl repository :: Aff (Either AjaxError (Array Tag))
  pure $ either (const []) ((<$>) _.name) resp

tagUrlApiUrl :: { user :: String, repo :: String } -> URL
tagUrlApiUrl { user, repo } = baseURL <> path
  where
  baseURL = "https://api.github.com"

  path = "/repos/" <> user <> "/" <> repo <> "/tags"
