{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module Html.Common.GitHub (Repo (..), Language (..), Licence (..), getRepos) where

import           Control.Monad.IO.Class
import           Data.Aeson
import           Data.Maybe
import           Data.Text              as T
import           GHC.Generics
import           Network.HTTP.Req
import           System.Environment
import qualified Data.ByteString.Char8 as B

data Language = LangHS
    | LangJS
    | LangHTML
    | LangPHP
    | LangASM
    | LangGNU
    | LangTS
    | LangCoffee
    | LangShell
    | LangGeneric
    | LangPython
    | LangBlitzBasic
    | LangC
    | LangTcl
    | LangVB
    | LangCPP
    | LangNix
    | LangDocker deriving (Eq, Generic, Show)

instance FromJSON Language where
    parseJSON (String a) = return $ case a of
        "JavaScript"   -> LangJS
        "HTML"         -> LangHTML
        "Python"       -> LangPython
        "PHP"          -> LangPHP
        "TypeScript"   -> LangTS
        "CoffeeScript" -> LangCoffee
        "Shell"        -> LangShell
        "Assembly"     -> LangASM
        "C"            -> LangC
        "Makefile"     -> LangShell
        "Visual Basic" -> LangVB
        "Dockerfile"   -> LangDocker
        "Tcl"          -> LangTcl
        "BlitzBasic"   -> LangBlitzBasic
        "Haskell"      -> LangHS
        "C++"          -> LangCPP
        "VBA"          -> LangVB
        "Vim script"   -> LangShell
        "Vue"          -> LangJS
        "Nix"          -> LangNix
        _              -> error $ "Unknown language: " <> T.unpack a
    parseJSON Null = return LangGeneric
    parseJSON _ = return LangGeneric

newtype Licence = Licence {
    spdx_id :: String
} deriving (Eq, FromJSON, Generic, Show)

data Repo = Repo {
    name        :: String,
    description :: Maybe String,
    fork        :: Bool,
    language    :: Language,
    source      :: Maybe String,
    website     :: Maybe String,
    licence     :: Maybe Licence,
    stars       :: Int
} deriving (Generic, Show)

instance FromJSON Repo where
    parseJSON (Object a) = do
        homepage <- a .: "homepage"
        -- private <- a .: "private"
        fork' <- a .: "fork"
        -- fullName <- a .: "full_name"
        url <- a .: "clone_url"
        -- issuesUrl <- a .: "issues_url"
        name' <- a .: "name"
        -- forksCount <- a .: "forks_count"
        -- updatedAt <- a .: "updated_at"
        language' <- a .: "language"
        -- pushedAt <- a .: "pushed_at"
        -- openIssuesCount <- a .: "open_issues_count"
        -- openIssues <- a .: "open_issues"
        -- watchers <- a .: "watchers"
        stargazers <- a .: "stargazers_count"
        licence' <- a .:? "license"
        -- licenceUrl <- licence .: "url"
        -- licenceKey <- licence .: "key"
        -- licenceName <- licence .: "name"
        -- licenceSpdxId <- licence .: "spdx_id"
        -- traceShowM licenceSpdxId
        -- forks <- a .: "forks"
        desc <- a .: "description"
        -- watchersCount <- a .: "watchers_count"

        let website' = if isJust homepage && Just "" == homepage
            then Nothing
            else homepage

        let licenceText = if isJust licence' && Just (Licence "NOASSERTION") == licence'
            then Nothing
            else licence'

        return $ Repo {
            name = name',
            description = desc,
            fork = fork',
            language = language',
            source = url,
            website = website',
            licence = licenceText,
            stars = stargazers
        }
    parseJSON _ = error "Wrong object"

getRepos ∷ Text → Req [Repo]
getRepos user = do
    githubAccessToken <- liftIO . getEnv $ "GITHUB_ACCESS_TOKEN" -- throws
    res <- req GET (https "api.github.com" /: "users" /: user /: "repos") NoReqBody jsonResponse (
        "per_page" =: ("100" :: Text) <>
        "sort" =: ("pushed" :: Text) <> -- can't sort by stars
        "type" =: ("owner" :: Text) <>
        "direction" =: ("desc" :: Text) <>
        header "User-Agent" "Dan's Haskell Bot v1.0" <>
        header "Authorization" (B.pack $ "Bearer " <> githubAccessToken)
        )
    return $ responseBody res
