{-# LANGUAGE BlockArguments, DeriveAnyClass, DeriveGeneric, NamedFieldPuns, OverloadedStrings #-}
module Main where

import AWSLambda.Events.APIGateway
import Control.Monad
import Data.Aeson hiding (object)
import Data.Aeson.Embedded
import Data.ByteString.Lazy.Base64
import Data.Char
import Data.Function ((&))
import Data.Maybe
import Data.Text as T
import Data.Text.Encoding
import Data.Time
import Data.Time.Format.ISO8601
import GHC.Generics
import GitHub.REST as GH hiding ((.:))
import Network.AWS.Data.Text
import Network.AWS.Data.Query
import Network.AWS.Lens

newtype RefObject = RefObject {
    sha :: Text
} deriving (FromJSON, Generic)

newtype Ref = Ref {
    object :: RefObject
} deriving (FromJSON, Generic)

data CommentRecord = CommentRecord {
    recName :: Text,
    recComment :: Text
} deriving (Generic, ToJSON)

instance FromJSON CommentRecord where
  parseJSON (Object o) = CommentRecord <$>
    o .: "name" <*>
    o .: "comment"
  parseJSON _ = error "Unacceptable comment record"

main :: IO ()
main = apiGatewayMain handler
  
handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler request = do
    print $ request ^. agprqHeaders
    print $ request ^. requestBody
    print $ parseQueryString $ encodeUtf8 $ fromMaybe "" $ request ^. requestBody
    {-}
    let state = GitHubState {
          token = Just (AccessToken "Not for you!")
        , userAgent = "danwdart/websites"
        , apiVersion = "v3"
    }
    branch <- branchName <$> getCurrentTime
    runGitHubT state $ do
        masterSHA <- getMasterSHA
        void $ createBranch masterSHA branch
        void $ commitNewFile branch postId commentId
        void $ pullRequest branch
    -}
    pure $ responseOK & agprsHeaders .~ [("Content-Type", "text/html")] & responseBody ?~ "<span style=\"color:green\">OK</span>"

-- Use GitHub API to submit a PR.
name :: Text
name = "Bob Dobbs"

email :: Text
email = "bob@bob.com"

comment :: Text
comment = "This is a sample comment."

postId :: Text
postId = "postId"

commentId :: Text
commentId = "commentId"

owner :: Text
owner = "danwdart"

repo :: Text
repo = "websites"

title :: Text
title = "New Comment from " <> name

baseBranch :: Text
baseBranch = "master"

branchName :: UTCTime -> Text
branchName utcTime = T.filter isDigit $ pack (iso8601Show utcTime)

commentRecord :: CommentRecord
commentRecord = CommentRecord {
    recName = name,
    recComment = comment
}

commentToPRMessage :: CommentRecord -> Text
commentToPRMessage CommentRecord { recName, recComment } = recName <> ": " <> recComment

getMasterSHA :: (MonadGitHubREST m) => m Text
getMasterSHA = sha . object <$> queryGitHub GHEndpoint
  { GH.method = GET
  , endpoint = "/repos/:owner/:repo/git/refs/heads/master"
  , endpointVals =
    [ "owner" := owner
    , "repo" := repo
    ]
  , ghData = []
  }

createBranch :: (MonadGitHubREST m) => Text -> Text -> m Value
createBranch fromSHA branch = queryGitHub GHEndpoint
  { GH.method = POST
  , endpoint = "/repos/:owner/:repo/git/refs"
  , endpointVals =
    [ "owner" := owner
    , "repo" := repo
    ]
  , ghData = 
    [
      "ref" := "refs/heads/" <> branch
    , "sha" := fromSHA
    ]
  }

commitNewFile :: (MonadGitHubREST m) => Text -> Text -> Text -> m Value
commitNewFile branch postId commentId = queryGitHub GHEndpoint
  { GH.method = PUT
  , endpoint = "/repos/:owner/:repo/contents/:path"
  , endpointVals =
    [ "owner" := owner
    , "repo" := repo
    , "path" := "posts/comments/" <> postId <> "/" <> commentId
    ]
  , ghData =
    [ "message" := title
    , "content" := encodeBase64 (encode commentRecord)
    , "branch"  := branch
    , "committer" := [
        "name" := name,
        "email" := email
    ]
  ]
  }

pullRequest :: (MonadGitHubREST m) => Text -> m Value
pullRequest branch = queryGitHub GHEndpoint
  { GH.method = POST
  , endpoint = "/repos/:owner/:repo/pulls"
  , endpointVals =
    [
      "owner" := owner
    , "repo" := repo
    ]
  , ghData = [
      "title" := title
    , "head" := branch
    , "base" := baseBranch
    , "body" := commentToPRMessage commentRecord
    , "maintainer_can_modify" := True
    , "draft" := False
  ]
  }

