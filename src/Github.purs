module Github where

import Prelude
import Affjax (URL)
import Affjax.RequestHeader (RequestHeader(..))
import Data.Either (Either, either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Partial.Unsafe (unsafeCrashWith)
import Simple.Ajax (AjaxError, HTTPError, getR, patchR_)

newtype GithubToken
  = GithubToken String

type Tag
  = { name :: String }

type TagName
  = String

type Repository
  = { repoUser :: String
    , repoName :: String
    }

type IssueContent
  = { title :: String
    , body :: String
    , state :: String
    }

requestTags :: GithubToken -> Repository -> Aff (Array TagName)
requestTags token repository = do
  resp <-
    getR
      { headers: [ apiHeaders token ] }
      (url repository) ::
      Aff (Either AjaxError (Array Tag))
  pure $ either (unsafeCrashWith <<< show) ((<$>) _.name) resp
  where
  url { repoUser, repoName } =
    baseURL
      <> "/repos/"
      <> repoUser
      <> "/"
      <> repoName
      <> "/tags"

requestPatchIssue :: GithubToken -> IssueContent -> Aff Unit
requestPatchIssue token issueContent = do
  resp <-
    patchR_
      { headers: [ apiHeaders token ] }
      url
      (Just issueContent) ::
      Aff (Either HTTPError Unit)
  pure $ either (unsafeCrashWith <<< show) (identity) resp
  where
  user = "andys8"

  repo = "package-sets"

  issue = "1"

  url = baseURL <> "/repos/" <> user <> "/" <> repo <> "/issues/" <> issue

baseURL :: URL
baseURL = "https://api.github.com"

apiHeaders :: GithubToken -> RequestHeader
apiHeaders (GithubToken token) = RequestHeader "Authorization" ("Bearer " <> token)
