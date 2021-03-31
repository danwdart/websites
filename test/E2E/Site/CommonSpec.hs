{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.CommonSpec where

import           Control.Concurrent               (forkIO, killThread,
                                                   threadDelay)
import           Control.Concurrent.Async         (mapConcurrently)
import           Control.Exception                (SomeException (SomeException),
                                                   try)
import           Control.Monad                    (when)
import           Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import           Data.Aeson                       (Object,
                                                   Value (Array, String),
                                                   object)
import Data.Env                                                   
import           Data.Functor.Compose             (Compose (Compose, getCompose))
import           Data.List                        (nub)
import Data.Map ((!))
import           Data.Maybe                       (catMaybes)
import           Data.Text                        (Text, unpack)
import           Data.Vector                      (fromList)
import           Network.HTTP.Client              (Manager,
                                                   Response (responseStatus),
                                                   httpNoBody, newManager,
                                                   parseRequest)
import           Network.HTTP.Client.TLS          (tlsManagerSettings)
import           Network.HTTP.Types.Status        (statusCode)
import qualified Site.Blog                        as B
import qualified Site.DanDart                     as D
import qualified Site.JolHarg                     as J
import qualified Site.M0ORI                       as M
import qualified Site.MadHacker                   as MH
import           System.Environment               (setEnv)
import           System.Process
import           System.Random                    (Random (randomRIO))
import           Test.Hspec                       (HasCallStack, Spec, describe,
                                                   hspec, it, runIO, shouldBe,
                                                   shouldSatisfy)
import           Test.Hspec.Expectations          (shouldNotBe,
                                                   shouldNotContain)
import           Test.WebDriver                   (Browser (chromeOptions),
                                                   Capabilities (browser),
                                                   Element,
                                                   Selector (ByCSS, ByClass),
                                                   WD,
                                                   WDConfig (wdCapabilities),
                                                   additionalCaps, attr, chrome,
                                                   click, currentWindow,
                                                   defaultCaps, defaultConfig,
                                                   elemSize, finallyClose,
                                                   findElem, findElems, getText,
                                                   openPage)
import           Test.WebDriver.Class             (WebDriver, methodPost)
import           Test.WebDriver.Commands.Internal (doWinCommand)
import           Test.WebDriver.Config            (WebDriverConfig)
import           Test.WebDriver.JSON              (pair)
import           Test.WebDriver.Monad             (runSession)

firefoxConfig ∷ WDConfig
firefoxConfig = defaultConfig {
    wdCapabilities = defaultCaps {
        additionalCaps = [
            ("moz:firefoxOptions", object [
                ("args", Array (fromList [String ""]))
            ])
        ]
    }
}

chromeConfig ∷ WDConfig
chromeConfig = defaultConfig {
    wdCapabilities = defaultCaps {
        browser = chrome {
            chromeOptions = []
        }
    }
}

-- |Set the dimensions of the current window.
setWindowSize ∷ (HasCallStack, WebDriver wd) ⇒ (Word, Word) → wd Object
setWindowSize = doWinCommand methodPost currentWindow "/size"
                . pair ("width", "height")

showTuple ∷ (Show a) ⇒ (a, a) → String
showTuple (a, b) = show a <> "x" <> show b

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
    --("Chrome", chromeConfig)
    ]

sites ∷ [(Text, IO ())]
sites = [
    ("blog", runReaderT B.serve (development ! "blog")),
    ("dandart", runReaderT D.serve (development ! "dandart")),
    ("jolharg", runReaderT J.serve (development ! "jolharg")),
    ("madhacker", runReaderT MH.serve (development ! "madhacker")),
    ("m0ori", runReaderT M.serve (development ! "m0ori"))
    ]

-- in terms of safeTry / try?
ioDef ∷ a → IO a → IO a
ioDef d io = either (\(SomeException _) -> d) id <$> try io

testForLink ∷ Element → WD ()
testForLink linkToClick = do
    linkName <- getText linkToClick
    cardSizes <- do
        click linkToClick
        cards <- findElems $ ByClass "card"
        mapM elemSize cards

    liftIO . hspec . describe (unpack linkName) . it "visible cards are only one size" $ (
        (length . nub . filter (/= (0, 0)) $ cardSizes )`shouldSatisfy` (< 2))

testForResolution ∷ Text → (Word, Word) → WD ()
testForResolution siteName winSize = do
    navHeight <- do
        _ <- setWindowSize winSize
        navbar <- findElem $ ByClass "navbar-nav"
        (_, navHeight) <- elemSize navbar
        pure navHeight

    liftIO . hspec $ describe (showTuple winSize) $
        it "nav height is equal to 39" $
            navHeight `shouldBe` 39

    -- only care about cards in JolHarg, but it's an option later.
    when ("jolharg" == siteName) $ do
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

testNotBroken ∷ (String, Int) → Spec
testNotBroken (url', status) = describe url' $ do
    it "should not be failing" $
        status `shouldNotBe` 0
    it "should not 404" $
        status `shouldNotBe` 404

testForConfig ∷ WebDriverConfig conf ⇒ Int → Text → (Text, conf) → Spec
testForConfig myPort siteName (configName, config) =
    describe (unpack configName) $ do
        runIO . runSession config . finallyClose $ do
            openPage $ "http://" <> unpack siteName <> ".localhost:" <> show myPort

            -- only the first option
            when ("Firefox" == configName) $ do
                urls <- do
                    as <- findElems (ByCSS "a[href^=http]")
                    hrefs <- mapM (`attr` "href") as
                    pure (catMaybes $ getCompose (unpack <$> Compose hrefs))

                images <- do
                    as <- findElems (ByCSS "img[src^=http]")
                    srcs <- mapM (`attr` "src") as
                    pure (catMaybes $ getCompose (unpack <$> Compose srcs))

                manager <- liftIO $ newManager tlsManagerSettings

                urlStatuses <- liftIO $ mapConcurrently (getStatuses manager) urls

                imageStatuses <- liftIO $ mapConcurrently (getStatuses manager) images

                liftIO . hspec $ do
                    describe "has no insecure images" $ mapM_ testSecureLink images
                    describe "has no broken links" $ mapM_ testNotBroken urlStatuses
                    describe "has no missing images" $ mapM_ testNotBroken imageStatuses

            mapM_ (testForResolution siteName) resolutions

testForSite ∷ (Text, IO ()) → Spec
testForSite (siteName, serve) = describe (unpack siteName) $ do
    myPort <- runIO (randomRIO (49152, 65535) :: IO Int)
    thread <- runIO $ do
        setEnv "PORT" (show myPort)
        thread <- forkIO serve
        threadDelay 3000000 -- Let it start
        pure thread

    mapM_ (testForConfig myPort siteName) configs
    runIO $ killThread thread


spec ∷ Spec
spec = runIO $ withCreateProcess (shell "selenium-server") $ \_ _ _ _ -> do
    threadDelay 5000000
    hspec $ mapM_ testForSite sites
