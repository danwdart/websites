{-# LANGUAGE UnicodeSyntax #-}
module E2E.Site.CommonSpec where

main ∷ IO ()
main = pure ()

{-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.CommonSpec where

import           Control.Concurrent         (forkIO, killThread, threadDelay)
import           Control.Concurrent.Async   (mapConcurrently)
import           Control.Exception          (SomeException (SomeException), try)
import           Control.Monad              (unless, when)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Env                   as Env
import           Data.Env.Types             as Env
import           Data.Functor.Compose       (Compose (Compose, getCompose))
import           Data.List                  (nub)
import           Data.Maybe                 (catMaybes)
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import           Data.Text                  (Text, unpack)
import           Network.HTTP.Client        (Manager, Response (responseStatus),
                                             httpNoBody, newManager,
                                             parseRequest)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Status  (statusCode)
import           System.Environment         (setEnv)
import           System.Process
import           System.Random
import           Test.Hspec                 (Spec, describe, hspec, it, runIO,
                                             shouldBe, shouldSatisfy)
import           Test.Hspec.Expectations    (shouldNotBe, shouldNotContain)
import           Test.WebDriver             (Browser (chromeOptions),
                                             Capabilities (browser), Element,
                                             Rect (..), Selector (..), WD,
                                             WDConfig (wdCapabilities), attr,
                                             chrome, click, defaultCaps,
                                             defaultConfig, elemRect,
                                             finallyClose, findElem, findElems,
                                             getText, openPage, setWindowRect)
import           Test.WebDriver.Class       (WebDriver)
import           Test.WebDriver.Config      (WebDriverConfig)
import           Test.WebDriver.Monad       (runSession)

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

chromeConfig ∷ WDConfig
chromeConfig = defaultConfig {
    wdCapabilities = defaultCaps {
        browser = chrome {
            -- chromeBinary = Just $(staticWhich "google-chrome-stable"),
            chromeOptions = []
        }
    }
}

showTuple ∷ (Show a) ⇒ (a, a) → String
showTuple (a, b) = show a <> "x" <> show b

resolutions ∷ [(Float, Float)]
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
    ("Firefox", firefoxConfig),
    ("Chrome", chromeConfig)
    ]

sites ∷ Set (Text, IO ())
sites = S.mapMonotonic (
        \website -> (
            slug website,
            runReaderT ((Env.serve :: Website -> WebsiteIO ()) (website :: Website) :: WebsiteIO ()) website :: IO ()
        )
    ) development

-- in terms of safeTry / try?
ioDef ∷ a → IO a → IO a
ioDef d io = either (\(SomeException _) -> d) id <$> try io

-- @TODO use the real elemRect and remove this
elemSize ∷ WebDriver m ⇒ Element → m (Float, Float)
elemSize x = do
    r <- elemRect x
    pure (rectWidth r, rectHeight r)

testForLink ∷ Element → WD ()
testForLink linkToClick = do
    linkName <- getText linkToClick
    cardSizes <- do
        click linkToClick
        cards <- findElems $ ByCSS "input:checked ~ .page .card"
        mapM elemSize cards

    liftIO . print $ cardSizes

    liftIO . hspec . describe (unpack linkName) . it "visible cards are only one size" $ (
        (length . nub . filter (/= (0, 0)) $ cardSizes )`shouldSatisfy` (< 2))

testForResolution ∷ (Float, Float) → WD ()
testForResolution winSize@(width, height) = do
    let rect = Rect {
        rectX = 0,
        rectY = 0,
        rectWidth = width,
        rectHeight = height
    }
    navHeight <- do
        _ <- setWindowRect rect
        navbar <- findElem $ ByCSS ".navbar-nav"
        (_, navHeight) <- elemSize navbar
        pure navHeight

    ((liftIO . hspec) . describe (showTuple winSize))
        . it "nav height is equal to 39"
            $ (navHeight `shouldBe` 39)

    links <- findElems $ ByCSS ".navbar-nav label a"
    mapM_ testForLink links

testSecureLink ∷ String → Spec
testSecureLink src = describe src .
    it "is secure" $
        src `shouldNotContain` "http:"

getStatuses ∷ Manager → String → IO (String, Int)
getStatuses manager url' = ioDef (url', 0) $ do
    request <- parseRequest url'
    response <- httpNoBody request manager
    pure (url', statusCode . responseStatus $ response)

brokenExceptions ∷ [String]
brokenExceptions = [
    "http://dandart.localhost:8080", -- appears in tests only
    "http://blog.localhost:8080", -- appears in tests only
    "http://madhacker.localhost:8080", -- appears in tests only
    "http://jolharg.localhost:8080", -- appears in tests only
    "http://m0ori.localhost:8080", -- appears in tests only
    "https://canddi.com", -- resolves to 0.0.0.0 with adguard
    "https://www.linuxvoice.com/category/podcasts/", -- self-signed certificate
    "https://upload.wikimedia.org/wikipedia/commons/6/6a/JavaScript-logo.png",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Antu_bash.svg/512px-Antu_bash.svg.png",
    "https://upload.wikimedia.org/wikipedia/commons/3/3b/C.sh-600x600.png",
    "https://web.archive.org/web/20181125122112if_/https://upload.wikimedia.org/wikipedia/commons/1/1a/Code.jpg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/HTML5_logo_and_wordmark.svg/512px-HTML5_logo_and_wordmark.svg.png",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/PHP-logo.svg/711px-PHP-logo.svg.png",
    "https://upload.wikimedia.org/wikipedia/commons/0/0a/Python.svg",
    "https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/1280px-Haskell-Logo.svg.png"
    ]

testNotBroken ∷ (String, Int) → Spec
testNotBroken (url', status) = describe url' .
    unless (url' `elem` brokenExceptions) $ do
    -- it "should not be failing" $
    --     status `shouldNotBe` 0
    it "should not 404" $
        status `shouldNotBe` 404

testForConfig ∷ WebDriverConfig conf ⇒ Int → Text → (Text, conf) → Spec
testForConfig myPort siteName (configName, config) = describe (unpack siteName) .
    describe (unpack configName) $ do
        runIO . runSession config . finallyClose $ do
            liftIO . putStrLn $ "Opening page"
            openPage $ "http://" <> unpack siteName <> ".localhost:" <> show myPort

            liftIO . putStrLn $ "Opened page"
            -- only the first option
            when ("Firefox" == configName) $ do
                urls' <- do
                    liftIO . putStrLn $ "Finding external links"
                    as <- findElems (ByCSS "a[href^=http]")
                    liftIO . putStrLn $ "Going through external links"
                    hrefs <- mapM (`attr` "href") as
                    pure (catMaybes $ getCompose (unpack <$> Compose hrefs))

                images <- do
                    as <- findElems (ByCSS "img[src^=http]")
                    srcs <- mapM (`attr` "src") as
                    pure (catMaybes $ getCompose (unpack <$> Compose srcs))

                manager <- liftIO $ newManager tlsManagerSettings

                urlStatuses <- liftIO $ mapConcurrently (getStatuses manager) urls'

                imageStatuses <- liftIO $ mapConcurrently (getStatuses manager) images

                liftIO . hspec $ do
                    describe "has no insecure images" $ mapM_ testSecureLink images
                    describe "has no broken links" $ mapM_ testNotBroken urlStatuses
                    describe "has no missing images" $ mapM_ testNotBroken imageStatuses

            mapM_ testForResolution resolutions

testForSite ∷ (Text, IO ()) → Spec
testForSite (siteName, serve') = describe (unpack siteName) $ do
    myPort <- runIO (randomRIO (49152, 65535) :: IO Int)
    thread <- runIO $ do
        setEnv "PORT" (show myPort)
        thread <- forkIO serve'
        threadDelay 5000000 -- Let it start
        pure thread

    mapM_ (testForConfig myPort siteName) configs
    runIO $ killThread thread


spec ∷ Spec
spec = runIO . withCreateProcess (shell "selenium-server -role hub") $ \_ _ _ _ ->
    withCreateProcess (shell "selenium-server -role node") $ \_ _ _ _ -> do
        threadDelay 5000000
        hspec $ mapM_ testForSite (S.toList sites)
-}
