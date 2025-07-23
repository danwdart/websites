{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE QuasiQuotes        #-}

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
import Network.URI.Static
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

languageToURI :: Language -> URI
languageToURI = \case
    LangASM -> [uri|https://upload.wikimedia.org/wikipedia/commons/thumb/f/f3/Motorola_6800_Assembly_Language.png/800px-Motorola_6800_Assembly_Language.png|]
    LangBlitzBasic -> [uri|https://upload.wikimedia.org/wikipedia/en/6/65/BlitzBasicLogo.gif|]
    LangC -> [uri|https://upload.wikimedia.org/wikipedia/commons/3/3b/C.sh-600x600.png|]
    LangCoffee -> [uri|https://farm8.staticflickr.com/7212/7168325292_16a46a1fea_n.jpg|]
    LangCPP -> [uri|https://upload.wikimedia.org/wikipedia/commons/1/18/ISO_C%2B%2B_Logo.svg|]
    LangDocker -> [relativeReference|/img/docker-mark-blue.svg|]
    LangGeneric -> [uri|https://web.archive.org/web/20181125122112if_/https://upload.wikimedia.org/wikipedia/commons/1/1a/Code.jpg|]
    LangHS -> [uri|https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/1280px-Haskell-Logo.svg.png|]
    LangHTML -> [uri|https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/HTML5_logo_and_wordmark.svg/512px-HTML5_logo_and_wordmark.svg.png|]
    LangJS -> [uri|https://upload.wikimedia.org/wikipedia/commons/6/6a/JavaScript-logo.png|]
    LangNix -> [uri|https://raw.githubusercontent.com/NixOS/nixos-artwork/refs/heads/master/logo/nix-snowflake-colours.svg|]
    LangPHP -> [uri|https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/PHP-logo.svg/711px-PHP-logo.svg.png|]
    LangPython -> [uri|https://upload.wikimedia.org/wikipedia/commons/0/0a/Python.svg|]
    LangShell -> [uri|https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Antu_bash.svg/512px-Antu_bash.svg.png|]
    LangTcl -> [uri|https://upload.wikimedia.org/wikipedia/commons/4/41/Tcl.svg|]
    LangTS -> [uri|https://rynop.files.wordpress.com/2016/09/ts.png?w=200|]
    LangVB -> [uri|https://upload.wikimedia.org/wikipedia/en/e/e4/Visual_Basic_6.0_logo.png|]

displayLanguage :: Language -> Text
displayLanguage = \case
    LangASM -> "Assembly language"
    LangBlitzBasic -> "Blitz Basic"
    LangC -> "C"
    LangCoffee -> "CoffeeScript"
    LangCPP -> "C++"
    LangDocker -> "Docker"
    LangGeneric -> "Unknown/Generic"
    LangHS -> "Haskell"
    LangHTML -> "HTML"
    LangJS -> "JS"
    LangNix -> "Nix"
    LangPHP -> "PHP"
    LangPython -> "Python"
    LangShell -> "Shell script"
    LangTcl -> "Tcl"
    LangTS -> "TypeScript"
    LangVB -> "Visual Basic"

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
  deriving FromJSON via Generically Licence

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
