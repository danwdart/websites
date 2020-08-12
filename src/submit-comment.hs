{-# LANGUAGE BlockArguments, DeriveAnyClass, DeriveGeneric, NamedFieldPuns, OverloadedStrings #-}
module Main where

import AWSLambda.Events.APIGateway
import Control.Monad
import Data.Aeson hiding (object)
import Data.Aeson.Embedded
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
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
import System.Environment

newtype RefObject = RefObject {
    sha :: Text
} deriving (FromJSON, Generic)

newtype Ref = Ref {
    object :: RefObject
} deriving (FromJSON, Generic)

data CommentRecord = CommentRecord {
    recName :: Text,
    recComment :: Text,
    recEmail :: Text
} deriving (Generic, ToJSON)

instance FromJSON CommentRecord where
  parseJSON (Object o) = CommentRecord <$>
    o .: "name" <*>
    o .: "comment" <*>
    o .: "email"
  parseJSON _ = error "Unacceptable comment record"

owner :: Text
owner = "danwdart"

repo :: Text
repo = "websites"

title :: Text -> Text
title = ("New Comment from " <>)

baseBranch :: Text
baseBranch = "master"

branchName :: UTCTime -> Text
branchName utcTime = T.filter isDigit $ pack (iso8601Show utcTime)

main :: IO ()
main = apiGatewayMain handler

lookupQueryString :: QueryString -> ByteString -> ByteString
lookupQueryString qs key = 
  (\[QPair a (QValue (Just b))] -> b) $
  Prelude.filter (\(QPair a b) -> a == key) $
  (\(QList x) -> x) qs
  
handler :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
handler request = do
    -- print $ request ^. agprqHeaders
    --print $ request ^. requestBody
    githubAccessToken <- getEnv "GITHUB_ACCESS_TOKEN"
    let qs = parseQueryString $ encodeUtf8 $ fromMaybe "" $ request ^. requestBody
    let lookupQS = lookupQueryString qs
    let name = decodeUtf8 $ lookupQS "name"
    let email = decodeUtf8 $ lookupQS "email"
    let comment = decodeUtf8 $ lookupQS "comment"
    let postId = decodeUtf8 $ lookupQS "postId"
    commentId <- T.pack . show <$> getCurrentTime
    let commentRecord = CommentRecord name comment email
    let state = GitHubState {
          token = Just (AccessToken $ B.pack githubAccessToken)
        , userAgent = "danwdart/websites"
        , apiVersion = "v3"
    }
    branch <- branchName <$> getCurrentTime
    runGitHubT state $ do
        masterSHA <- getMasterSHA
        void $ createBranch masterSHA branch
        void $ commitNewFile branch postId commentId commentRecord
        void $ pullRequest branch commentRecord
    pure $ responseOK & agprsHeaders .~ [("Content-Type", "text/html")] & responseBody ?~ "<span style=\"color:green\">OK</span>"

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

commitNewFile :: (MonadGitHubREST m) => Text -> Text -> Text -> CommentRecord -> m Value
commitNewFile branch postId commentId commentRecord = queryGitHub GHEndpoint
  { GH.method = PUT
  , endpoint = "/repos/:owner/:repo/contents/:path"
  , endpointVals =
    [ "owner" := owner
    , "repo" := repo
    , "path" := "posts/comments/" <> postId <> "/" <> commentId
    ]
  , ghData =
    [ "message" := title (recName commentRecord)
    , "content" := encodeBase64 (encode commentRecord)
    , "branch"  := branch
    , "committer" := [
        "name" := recName commentRecord,
        "email" := recEmail commentRecord
    ]
  ]
  }

pullRequest :: (MonadGitHubREST m) => Text -> CommentRecord -> m Value
pullRequest branch commentRecord = queryGitHub GHEndpoint
  { GH.method = POST
  , endpoint = "/repos/:owner/:repo/pulls"
  , endpointVals =
    [
      "owner" := owner
    , "repo" := repo
    ]
  , ghData = [
      "title" := title (recName commentRecord)
    , "head" := branch
    , "base" := baseBranch
    , "body" := commentToPRMessage commentRecord
    , "maintainer_can_modify" := True
    , "draft" := False
  ]
  }

