{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists #-}
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
import Data.Aeson
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
firefoxConfig = defaultConfig {
    wdCapabilities = defaultCaps {
        additionalCaps = [
            ("moz:firefoxOptions", object [
                ("args", Array [String "--headless"])
            ])
        ]
    }
}

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

-- stats stolen from https://www.browserstack.com/guide/common-screen-resolutions
-- heck we could probably use them at some point
resolutions ∷ [(Word, Word)]
resolutions = [
    (320, 480), -- smallest common portrait mobile
    (360, 480),
    (360, 780), -- #6 portrait mobile
    (360, 800), -- #1 portrait mobile
    (390, 844), -- #2 portrait mobile
    (393, 873), -- #3 portrait mobile
    (412, 915), -- #4 portrait mobile
    (414, 896), -- #5 portrait mobile
    (480, 320), -- smallest common landscape mobile
    (480, 360),
    (480, 720),
    (601, 952), -- #6 portrait tablet
    (720, 480),
    (720, 1280),
    (768, 1024), -- #1 portrait tablet, smallest common portrait tablet
    (780, 360), -- #6 landscape mobile
    (800, 360), -- #1 landscape mobile
    (800, 1280), -- #5 portrait tablet
    (810, 1080), -- #2 portrait tablet
    (820, 1180), -- #3 portrait tablet
    (844, 390), -- #2 landscape mobile
    (873, 393), -- #3 landscape mobile
    (896, 414), -- #5 landscape mobile
    (900, 1440),
    (915, 412), -- #4 landscape mobile
    (952, 601), -- #6 landscape tablet
    (1024, 768), -- smallest common desktop and landscape tablet, #1 landscape tablet
    (1080, 810), -- #2 landscape tablet, smallest common landscape tablet
    (1080, 1920),
    (1180, 820), -- #3 landscape tablet
    (1280, 720), -- #4 desktop
    (1280, 800), -- #4 landscape tablet
    (1366, 768), -- #2 desktop
    (1440, 900), -- #5 desktop
    (1440, 3200), -- biggest common portrait mobile
    (1536, 864), -- #3 desktop
    (1600, 900), -- #6 desktop
    (1600, 2560), -- biggest common portrait tablet
    (1920, 1080), -- #1 desktop
    (2560, 1600), -- biggest common landscape tablet
    (3200, 1440), -- biggest common landscape mobile
    (5120, 2880) -- biggest common desktop
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
        setWindowSize (width, height)
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
    unless (src `elem` insecureExceptions) $ do
    it "is secure" $
        src `shouldStartWith` "https:"

getStatuses ∷ Manager → String → IO (String, Int)
getStatuses manager url' = ioDef (url', 0) $ do
    request <- parseRequest url'
    let req' = request {
        requestHeaders = [
            ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:133.0) Gecko/20100101 Firefox/133.0")
        ]
    }
    response <- httpLbs req' manager
    pure (url', statusCode . responseStatus $ response)

insecureExceptions ∷ [String]
insecureExceptions = [
    "http://xn--101-8cd4f0b.xn--p1ai/user/mouseriver12/", -- a commenter url
    "http://sauerbraten.org/", -- no ssl yet
    "http://ppa.launchpad.net/jonabeck/ppa/ubuntu", -- PPAs are verified separately
    "http://riscos.com/riscos/310/index.php" -- not available securely
    ]

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
    "https://draft.blogger.com/profile/03017160325987430398",
    "https://blocked-for-spam.com/", -- default user url for comments when they're spam
    "http://xn--101-8cd4f0b.xn--p1ai/user/mouseriver12/", -- some user's url which is only http
    "https://web.archive.org/web/20090211204719/https://www.pcworld.com/article/129977/how_to_reinstall_windows_xp.html", -- no idea why
    "https://web.archive.org/web/20090619065522/http://www.daniweb.com/blogs/entry3288.html", -- no idea
    "https://web.archive.org/web/20120226075352/http://www.kumailht.com/blog/linux/10-features-ubuntu-should-implement/", -- dunno
    "https://web.archive.org/web/20100107134808/http://xenon.kevinghadyani.com/",
    "https://web.archive.org/web/20100224082039/https://xenon.kevinghadyani.com/desktop",
    "https://web.archive.org/web/20100211151037/http://matt.colyer.name:80/projects/iphone-linux/?title=Main_Page", -- archive is weird
    "https://web.archive.org/web/20140908152050/http://www.freebsd.org/doc/handbook/filesystems-zfs.html",
    "https://www.deepburner.com/", -- ????
    "https://www.linkedin.com/in/dandart", -- why 999???
    "https://viewex.co.uk/", -- either no longer active or blocked
    "https://cloudbanter.com/", -- either no longer active or blocked
    "https://docs.dadi.cloud/", -- either no longer active or blocked
    "https://www.soampli.com/" -- it's fine though???
    ]

testNotBroken ∷ (String, Int) → Spec
testNotBroken (url', status) = describe url' .
    unless (url' `elem` brokenExceptions) $ do
    -- it "should not be failing" $
    --     status `shouldNotBe` 0
    it "should 200" $
        status `shouldBe` 200
    -- it "should not 404" $
    --     status `shouldNotBe` 404

testForConfig ∷ Website → (Text, WDConfig) → Spec
testForConfig website (configName, config) = describe (T.unpack (website ^. slug)) .
    describe (T.unpack configName) $ do
        runIO . wrappedRunSession config . finallyClose $ do
            liftIO . putStrLn $ "Opening page"

            -- Open the dev only pages
            openPage $ website ^. baseUrl . to show . to T.pack . to (T.replace "https://" "https://dev.") . to T.unpack

            liftIO . putStrLn $ "Opened page"
            -- only the first option - we don't need the following duplicated
            when ("Firefox" == configName) $ do
                urls' <- do
                    liftIO . putStrLn $ "Finding external links"
                    as <- findElems (ByCSS "a[href^=http]")
                    liftIO . putStrLn $ "Going through external links"
                    hrefs <- traverse (`attr` "href") as
                    -- TODO witherable or something
                    pure (catMaybes $ getCompose (T.unpack <$> Compose hrefs))

                liftIO . putStrLn $ "Found " <> show (length urls') <> " urls."

                images <- do
                    as <- findElems (ByCSS "img[src^=http]")
                    liftIO . putStrLn $ "Going through external images"
                    srcs <- traverse (`attr` "src") as
                    -- TODO witherable or something
                    pure (catMaybes $ getCompose (T.unpack <$> Compose srcs))

                liftIO . putStrLn $ "Found " <> show (length images) <> " images."

                liftIO . putStrLn $ "Creating a manager"

                manager <- liftIO $ newManager tlsManagerSettings

                liftIO . putStrLn $ "Getting URL statuses"

                urlStatuses <- liftIO $ mapConcurrently (getStatuses manager) urls'

                liftIO . putStrLn $ "Getting image statuses"

                imageStatuses <- liftIO $ mapConcurrently (getStatuses manager) images

                liftIO . putStrLn $ "Performing tests"

                liftIO . hspec $ do
                    describe "has no insecure images" $ traverse_ testSecureLink images
                    describe "has no insecure links" $ traverse_ testSecureLink urls'
                    describe "has no broken links" $ traverse_ testNotBroken urlStatuses
                    describe "has no missing images" $ traverse_ testNotBroken imageStatuses

            liftIO . putStrLn $ "Testing for each resolution"

            traverse_ testForResolution resolutions

testForSite ∷ Website → Spec
testForSite website = describe (T.unpack (website ^. slug)) $
    traverse_ (testForConfig website) configs

spec ∷ Spec
spec = runIO . hspec $ traverse_ testForSite production
