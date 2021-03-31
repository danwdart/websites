{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString.Char8       as B
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import           GitHub.REST                 as GH hiding ((.:))
import System.Environment

owner, repo :: Text
owner = "danwdart"
repo = "websites"

data Pull = Pull {
  number :: Int,
  body :: Text
} deriving (Generic, FromJSON, ToJSON, Show)

getAllPulls :: GitHubT IO [Pull]
getAllPulls = queryGitHub GHEndpoint {
    GH.method = GET,
    endpoint = "/repos/:owner/:repo/pulls?per_page=100",
    endpointVals = [
        "owner" := owner,
        "repo" := repo
    ],
    ghData = []
}

deletePull :: Int -> GitHubT IO Pull
deletePull n = queryGitHub GHEndpoint {
  GH.method = PATCH,
  endpoint = "/repos/:owner/:repo/pulls/" <> T.pack (show n), -- number
  endpointVals = [
      "owner" := owner,
      "repo" := repo
  ],
  ghData = [
      "state" := ("closed" :: Text)
  ]
}

main :: IO ()
main = do
    githubAccessToken <- getEnv "GITHUB_ACCESS_TOKEN"
    let state = GitHubState {
          token = Just (AccessToken $ B.pack githubAccessToken)
        , userAgent = "danwdart/websites"
        , apiVersion = "v3"
    }
    allPulls <- runGitHubT state getAllPulls
    
    let spamPulls = number <$> filter (\x ->
            "Amoxi" `T.isInfixOf` body x ||
            "Como puedo iniciar sesion" `T.isInfixOf` body x ||
            "Esumec" `T.isInfixOf` body x ||
            "Aginxo" `T.isInfixOf` body x
          ) allPulls

    mapM_ (\n -> do
      putStrLn $ "Going to delete PR " <> show n
      _ <- runGitHubT state $ deletePull n
      putStrLn $ "Deleted " <> show n
      ) spamPulls

    -- filter by which are spam - search?
    
      {- Delete branch?
    queryGitHub GHEndpoint
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
    -}
    -- get it and then delete branch by ref head.ref