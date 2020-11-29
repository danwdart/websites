{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.CommonSpec where

import           Control.Concurrent               (forkIO, killThread,
                                                   threadDelay)
import           Control.Concurrent.Async         (mapConcurrently)
import           Control.Exception                (SomeException (SomeException),
                                                   try)
import           Control.Monad                    (when)
import           Data.Aeson                       (Object,
                                                   Value (Array, String),
                                                   object)
import           Data.Functor.Compose             (Compose (Compose, getCompose))
import           Data.List                        (nub)
import           Data.Maybe                       (catMaybes)
import           Data.Text                        (unpack)
import           Data.Vector                      (fromList)
import           Network.HTTP.Client              (Response (responseStatus),
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
import           System.Random                    (Random (randomRIO))
import           Test.Hspec                       (HasCallStack, Spec, describe,
                                                   it, parallel, runIO,
                                                   shouldBe, shouldSatisfy)
import           Test.Hspec.Expectations          (shouldNotBe,
                                                   shouldNotContain)
import           Test.WebDriver                   (Browser (chromeOptions),
                                                   Capabilities (browser),
                                                   Selector (ByCSS, ByClass),
                                                   WDConfig (wdCapabilities),
                                                   additionalCaps, attr, chrome,
                                                   click, closeSession,
                                                   currentWindow, defaultCaps,
                                                   defaultConfig, elemSize,
                                                   findElem, findElems, getText,
                                                   openPage)
import           Test.WebDriver.Class             (WebDriver, methodPost)
import           Test.WebDriver.Commands.Internal (doWinCommand)
import           Test.WebDriver.JSON              (pair)
import           Test.WebDriver.Monad             (runSession, runWD)
import           Test.WebDriver.Session           (WDSessionState (getSession))

firefoxConfig ∷ WDConfig
firefoxConfig = defaultConfig {
    wdCapabilities = defaultCaps {
        additionalCaps = [
            ("moz:firefoxOptions", object [
                ("args", Array (fromList [String "--headless"]))
            ])
        ]
    }
}

chromeConfig ∷ WDConfig
chromeConfig = defaultConfig {
    wdCapabilities = defaultCaps {
        browser = chrome {
            chromeOptions = ["--headless"]
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

configs ∷ [(String, WDConfig)]
configs = [
    ("Firefox", firefoxConfig),
    ("Chrome", chromeConfig)
    ]

sites ∷ [(String, IO ())]
sites = [
    ("blog", B.serve),
    ("dandart", D.serve),
    ("jolharg", J.serve),
    ("madhacker", MH.serve),
    ("m0ori", M.serve)
    ]

-- in terms of safeTry / try?
ioDef ∷ a → IO a → IO a
ioDef d io = either (\(SomeException _) -> d) id <$> try io

spec ∷ Spec
spec = parallel $ mapM_ (\(siteName, serve) ->
    parallel . describe siteName $ do
        myPort <- runIO (randomRIO (49152, 65535) :: IO Int)
        thread <- runIO $ do
            setEnv "PORT" (show myPort)
            thread <- forkIO serve
            threadDelay 3000000 -- Let it start
            pure thread

        mapM_ (\(configName, config) -> do
            parallel . describe configName $ do
                session <- runIO . runSession config $ do
                    openPage $ "http://" <> siteName <> ".localhost:" <> show myPort
                    getSession

                -- only the first option
                when ("Firefox" == configName) $ do
                    urls <- runIO . runWD session $ do
                        as <- findElems (ByCSS "a[href^=http]")
                        hrefs <- mapM (`attr` "href") as
                        pure (catMaybes $ getCompose (unpack <$> Compose hrefs))

                    images <- runIO . runWD session $ do
                        as <- findElems (ByCSS "img[src^=http]")
                        srcs <- mapM (`attr` "src") as
                        pure (catMaybes $ getCompose (unpack <$> Compose srcs))

                    parallel . describe "has no insecure images" $ mapM_ (\src ->
                        describe src .
                            it "is not insecure" $
                                src `shouldNotContain` "http:"
                        ) images

                    manager <- runIO $ newManager tlsManagerSettings

                    urlStatuses <- runIO $ mapConcurrently (\url -> ioDef (url, 0) $ do
                        request <- parseRequest url
                        response <- httpNoBody request manager
                        pure (url, statusCode . responseStatus $ response)
                        ) urls

                    imageStatuses <- runIO $ mapConcurrently (\src -> ioDef (src, 0) $ do
                        request <- parseRequest src
                        response <- httpNoBody request manager
                        pure (src, statusCode . responseStatus $ response)
                        ) images

                    parallel . describe "has no broken links" $ mapM_ (\(url, status) ->
                        parallel . describe url $ do
                            it "should not be failing" $
                                status `shouldNotBe` 0
                            it "should not 404" $
                                status `shouldNotBe` 404
                        ) urlStatuses

                    parallel . describe "has no missing images" $ mapM_ (\(src, status) ->
                        parallel . describe src $ do
                            it "should not be failing" $
                                status `shouldNotBe` 0
                            it "should not 404" $
                                status `shouldNotBe` 404
                        ) imageStatuses

                mapM_ (\winSize -> do
                    describe (showTuple winSize) $ do
                        navHeight <- runIO . runWD session $ do
                            _ <- setWindowSize winSize
                            navbar <- findElem $ ByClass "navbar-nav"
                            (_, navHeight) <- elemSize navbar
                            pure navHeight

                        it "nav height is equal to 39" $
                            navHeight `shouldBe` 39

                        -- only care about cards in JolHarg, but it's an option later.
                        when ("jolharg" == siteName) $ do
                            links <- runIO . runWD session . findElems $ ByCSS ".navbar-nav label a"
                            mapM_ (\linkToClick -> do
                                linkName <- runIO . runWD session $ getText linkToClick
                                cardSizes <- runIO . runWD session $ do
                                    click linkToClick
                                    cards <- findElems $ ByClass "card"
                                    mapM elemSize cards

                                parallel . describe (unpack linkName) . it "visible cards are only one size" $ (
                                    (length . nub . filter (/= (0, 0)) $ cardSizes )`shouldSatisfy` (< 2))
                                ) links
                        ) resolutions
                runIO . runWD session $ closeSession
                ) configs
        runIO $ killThread thread
    ) sites
