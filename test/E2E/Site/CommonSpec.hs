{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.CommonSpec where

-- import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
-- import Control.Monad.Reader
import Data.Env                  as Env
import Data.Env.Types            as Env
import Data.Foldable
import Data.Functor.Compose
import Data.List                 qualified as L
import Data.Maybe
-- import Data.Set                  (Set)
-- import Data.Set                  qualified as S
import Data.Text                 (Text)
import Data.Text                 qualified as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
-- import System.Environment
-- import System.Process
-- import System.Random
import Test.Hspec
-- import Test.Hspec.Expectations
import Test.WebDriver
-- import Test.WebDriver.Class
-- import Test.WebDriver.Config
-- import Test.WebDriver.Monad
import Test.WebDriverWrapper

firefoxConfig ∷ WDConfig
firefoxConfig = defaultConfig {-} {
    wdCapabilities = defaultCaps {
        additionalCaps = [
            ("moz:firefoxOptions", object [
                ("args", Array (fromList [String ""]))
            ])
        ]
    }
}-}

-- chromeConfig ∷ WDConfig
-- chromeConfig = defaultConfig {
--     wdCapabilities = defaultCaps {
--         browser = chrome {
--             -- chromeBinary = Just $(staticWhich "google-chrome-stable"),
--             chromeOptions = []
--         }
--     }
-- }

showResolution ∷ (Show a) ⇒ (a, a) → String
showResolution (a, b) = show a <> "x" <> show b

resolutions ∷ [(Word, Word)]
resolutions = [
    (360, 480),
    (480, 360),
    (480, 720),
    (720, 480),
    (720, 1280),
    (768, 1024),
    (900, 1440),
    (1024, 768),
    (1080, 1920),
    (1280, 720),
    (1440, 900),
    (1920, 1080)
    ]

configs ∷ [(Text, WDConfig)]
configs = [
    ("Firefox", firefoxConfig)
    -- ("Chrome", chromeConfig)
    ]

-- in terms of safeTry / try?
ioDef ∷ a → IO a → IO a
ioDef d io = either (\(SomeException _) -> d) id <$> try io

testForLink ∷ Element → WD ()
testForLink linkToClick = do
    linkName <- getText linkToClick
    cardSizes <- fmap (bimap (round :: Float → Int) (round :: Float → Int)) <$> do
        click linkToClick
        cards <- findElems $ ByCSS "input:checked ~ .page .card"
        traverse elemSize cards

    liftIO . print $ cardSizes

    liftIO . hspec . describe (T.unpack linkName) . it "visible cards are only one size" $ (
        (length . L.nub . filter (/= (0, 0)) $ cardSizes )`shouldSatisfy` (< 2))

testForResolution ∷ (Word, Word) → WD ()
testForResolution winSize@(width, height) = do
    navHeight <- do
        _ <- setWindowSize (width, height)
        navbar <- findElem $ ByCSS ".navbar-nav"
        (_, navHeight) <- elemSize navbar
        pure navHeight

    ((liftIO . hspec) . describe (showResolution winSize))
        . it "nav height is equal to 40"
            $ (navHeight `shouldBe` 40)

    links <- findElems $ ByCSS ".navbar-nav label a"
    traverse_ testForLink links

testSecureLink ∷ String → Spec
testSecureLink src = describe src .
    it "is secure" $
        src `shouldNotContain` "http:"
        -- src `shouldContain` "https:"

getStatuses ∷ Manager → String → IO (String, Int)
getStatuses manager url' = ioDef (url', 0) $ do
    request <- parseRequest url'
    response <- httpNoBody request manager
    pure (url', statusCode . responseStatus $ response)

brokenExceptions ∷ [String]
brokenExceptions = [
    "https://canddi.com", -- resolves to 0.0.0.0 with adguard
    "https://www.linuxvoice.com/category/podcasts/", -- self-signed certificate
    "https://upload.wikimedia.org/wikipedia/commons/6/6a/JavaScript-logo.png",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Antu_bash.svg/512px-Antu_bash.svg.png",
    "https://upload.wikimedia.org/wikipedia/commons/3/3b/C.sh-600x600.png",
    "https://web.archive.org/web/20181125122112if_/https://upload.wikimedia.org/wikipedia/commons/1/1a/Code.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/HTML5_logo_and_wordmark.svg/512px-HTML5_logo_and_wordmark.svg.png",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/PHP-logo.svg/711px-PHP-logo.svg.png",
    "https://upload.wikimedia.org/wikipedia/commons/0/0a/Python.svg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/1280px-Haskell-Logo.svg.png",
    "https://draft.blogger.com/profile/11404839970927435985",
    "https://draft.blogger.com/profile/03812351582147993701",
    "https://draft.blogger.com/profile/15493119212604094040",
    "https://draft.blogger.com/profile/03017160325987430398"
    ]

testNotBroken ∷ (String, Int) → Spec
testNotBroken (url', status) = describe url' .
    unless (url' `elem` brokenExceptions) $ do
    -- it "should not be failing" $
    --     status `shouldNotBe` 0
    it "should not 404" $
        status `shouldNotBe` 404

testForConfig ∷ Website → (Text, WDConfig) → Spec
testForConfig website (configName, config) = describe (T.unpack (website ^. slug)) .
    describe (T.unpack configName) $ do
        runIO . wrappedRunSession config . finallyClose $ do
            liftIO . putStrLn $ "Opening page"

            -- Open the dev only pages
            openPage $ website ^. baseUrl . to show . to T.pack . to (T.replace "https://" "https://dev.") . to T.unpack

            liftIO . putStrLn $ "Opened page"
            -- only the first option
            when ("Firefox" == configName) $ do
                urls' <- do
                    liftIO . putStrLn $ "Finding external links"
                    as <- findElems (ByCSS "a[href^=http]")
                    liftIO . putStrLn $ "Going through external links"
                    hrefs <- traverse (`attr` "href") as
                    pure (catMaybes $ getCompose (T.unpack <$> Compose hrefs))

                images <- do
                    as <- findElems (ByCSS "img[src^=http]")
                    srcs <- traverse (`attr` "src") as
                    pure (catMaybes $ getCompose (T.unpack <$> Compose srcs))

                manager <- liftIO $ newManager tlsManagerSettings

                urlStatuses <- liftIO $ mapConcurrently (getStatuses manager) urls'

                imageStatuses <- liftIO $ mapConcurrently (getStatuses manager) images

                liftIO . hspec $ do
                    describe "has no insecure images" $ traverse_ testSecureLink images
                    describe "has no broken links" $ traverse_ testNotBroken urlStatuses
                    describe "has no missing images" $ traverse_ testNotBroken imageStatuses

            traverse_ testForResolution resolutions

testForSite ∷ Website → Spec
testForSite website = describe (T.unpack (website ^. slug)) $
    traverse_ (testForConfig website) configs

spec ∷ Spec
spec = runIO . hspec $ traverse_ testForSite production
