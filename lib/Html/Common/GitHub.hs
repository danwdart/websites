{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Html.Common.GitHub where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Types.Instances.NonEmpty ()
import Data.Maybe
import Data.Text.NonEmpty     (NonEmptyText)
import Data.NonEmpty          qualified as NE
import Data.Text              qualified as T
import Data.Text              (Text)
import Data.Text.Encoding     qualified as TE
import GHC.Generics
import Network.HTTP.Req
import Network.URI
import System.Environment

data Language = LangASM
    | LangBlitzBasic
    | LangC
    | LangCoffee
    | LangCPP
    | LangDocker
    | LangGeneric
    | LangHS
    | LangHTML
    | LangJS
    | LangNix
    | LangPHP
    | LangPython
    | LangShell
    | LangTcl
    | LangTS
    | LangVB
    deriving stock (Eq, Generic, Show)

instance FromJSON Language where
    parseJSON (String "Assembly") = pure LangASM
    parseJSON (String "BlitzBasic") = pure LangBlitzBasic
    parseJSON (String "C") = pure LangC
    parseJSON (String "CoffeeScript") = pure LangCoffee
    parseJSON (String "C++") = pure LangCPP
    parseJSON (String "Dockerfile") = pure LangDocker
    parseJSON (String "JavaScript") = pure LangJS
    parseJSON (String "Vue") = pure LangJS
    parseJSON (String "Haskell") = pure LangHS
    parseJSON (String "HTML") = pure LangHTML
    parseJSON (String "Nix") = pure LangNix
    parseJSON (String "Python") = pure LangPython
    parseJSON (String "PHP") = pure LangPHP
    parseJSON (String "Pug") = pure LangHTML
    parseJSON (String "Makefile") = pure LangShell
    parseJSON (String "Shell") = pure LangShell
    parseJSON (String "Stylus") = pure LangHTML
    parseJSON (String "Vim script") = pure LangShell
    parseJSON (String "Tcl") = pure LangTcl
    parseJSON (String "TypeScript") = pure LangTS
    parseJSON (String "VBA") = pure LangVB
    parseJSON (String "Visual Basic") = pure LangVB
    parseJSON (String "Visual Basic 6.0") = pure LangVB
    parseJSON (String a) = fail $ "Unknown language: " <> T.unpack a
    parseJSON Null = pure LangGeneric
    parseJSON _ = pure LangGeneric

newtype Licence = Licence {
    spdx_id :: NonEmptyText
} deriving stock (Eq, Generic, Show)
  deriving anyclass FromJSON

data Repo = Repo {
    name        :: NonEmptyText,
    description :: Maybe NonEmptyText,
    fork        :: Bool,
    language    :: Language,
    source      :: Maybe URI,
    website     :: Maybe URI,
    licence     :: Maybe Licence,
    stars       :: Int
} deriving stock (Generic, Show)

instance FromJSON Repo where
    parseJSON (Object a) = do
        homepage <- a .: "homepage"
        -- private <- a .: "private"
        fork' <- a .: "fork"
        -- fullName <- a .: "full_name"
        cloneUrl <- a .: "clone_url"
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

        let homepage' = case homepage of
                String ""   -> Nothing
                String url' -> parseURI (T.unpack url')
                _           -> Nothing

        let source' = case cloneUrl of
                String ""   -> Nothing
                String url' -> parseURI (T.unpack url')
                _           -> Nothing

        let licenceText = if isJust licence' && Just (Licence $ NE.trustedNonEmpty "NOASSERTION") == licence'
            then Nothing
            else licence'

        pure $ Repo {
            name = name',
            description = desc,
            fork = fork',
            language = language',
            source = (\src -> src { uriScheme = "https:" }) <$> source',
            website = (\ws -> ws { uriScheme = "https:" }) <$> homepage',
            licence = licenceText,
            stars = stargazers
        }
    parseJSON other = typeMismatch "Object" other

getRepos ∷ MonadHttp m ⇒ NonEmptyText → m [Repo]
getRepos user = do
    githubAccessToken <- liftIO . getEnv $ "GITHUB_ACCESS_TOKEN" -- throws
    res <- req GET (https "api.github.com" /: "users" /: NE.getNonEmpty user /: "repos") NoReqBody jsonResponse (
        "per_page" =: ("100" :: Text) <>
        "sort" =: ("pushed" :: Text) <> -- can't sort by stars
        "type" =: ("owner" :: Text) <>
        "direction" =: ("desc" :: Text) <>
        header "User-Agent" "Dan's Haskell Bot" <>
        header "Authorization" (TE.encodeUtf8 . T.pack $ "Bearer " <> githubAccessToken)
        )
    pure $ responseBody res
