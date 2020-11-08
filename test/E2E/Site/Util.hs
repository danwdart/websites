{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.Util where

import           Control.Concurrent               (forkIO, killThread,
                                                   threadDelay)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.Aeson                       (Object,
                                                   Value (Array, String),
                                                   object)
import           Data.Bifunctor                   (Bifunctor (bimap))
import           Data.Text                        (Text, unpack)
import           Data.Vector                      (fromList)
import           System.Directory                 (createDirectoryIfMissing)
import           System.Environment               (setEnv)
import           System.FilePath                  ((<.>), (</>))
import           System.Random                    (Random (randomRIO))
import           Test.Hspec                       (HasCallStack)
import           Test.WebDriver                   (Browser (chromeOptions),
                                                   Capabilities (browser),
                                                   Element,
                                                   Selector (ByCSS, ByClass),
                                                   WDConfig (wdCapabilities),
                                                   additionalCaps, chrome,
                                                   click, closeSession,
                                                   currentWindow, defaultCaps,
                                                   defaultConfig, elemSize,
                                                   findElem, findElems, getText,
                                                   openPage, runSession,
                                                   saveScreenshot)
import           Test.WebDriver.Class             (WebDriver, methodPost)
import           Test.WebDriver.Commands.Internal (doWinCommand)
import           Test.WebDriver.Config            (WebDriverConfig)
import           Test.WebDriver.JSON              (pair)

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

screenshotFile ∷ FilePath → FilePath → FilePath
screenshotFile screenshotDir screenshotFilename =
    screenshotDir </> screenshotFilename <.> "png"

configs ∷ [(String, WDConfig)]
configs = [
    ("Firefox", firefoxConfig),
    ("Chrome", chromeConfig)
    ]

runForLink ∷ (WebDriver m, MonadIO m) ⇒ FilePath → [Char] → Element → m (Text, [(Int, Int)])
runForLink screenshotDir res link = do
    linkName <- getText link
    let screenshotFilename = unpack linkName <> "-" <> res
    click link
    cards <- findElems $ ByClass "card"
    cardSizes <- mapM elemSize cards
    saveScreenshot $ screenshotFile screenshotDir screenshotFilename
    pure (linkName, fmap (bimap round round) cardSizes)


runForSize ∷ (MonadIO m, WebDriver m) ⇒ FilePath → (Word, Word) → m ((Word, Word), (Int, Int), [(Text, [(Int, Int)])])
runForSize screenshotDir winSize = do
    let res = showTuple winSize
    _ <- setWindowSize winSize

    let homeScreenshotFilename = "home-" <> res
    saveScreenshot $ screenshotFile screenshotDir homeScreenshotFilename

    navbar <- findElem $ ByClass "navbar-nav"
    navbarSize <- elemSize navbar

    links <- findElems $ ByCSS ".navbar-nav label a"
    cardSizes <- mapM (runForLink screenshotDir res) links

    pure (winSize, bimap round round navbarSize, cardSizes)


runForConfig ∷ (WebDriverConfig conf, Show a) ⇒ String → a → (FilePath, conf) → IO (FilePath, [((Word, Word), (Int, Int), [(Text, [(Int, Int)])])])
runForConfig siteName port (configName, config) = runSession config $ do
    let screenshotDir = "images" </> configName </> siteName
    liftIO . createDirectoryIfMissing True $ screenshotDir
    openPage $ "http://" <> siteName <> ".localhost:" <> show port
    results <- mapM (runForSize screenshotDir) resolutions
    closeSession
    pure (configName, results)


runTest ∷ String → IO () → IO [(FilePath, [((Word, Word), (Int, Int), [(Text, [(Int, Int)])])])]
runTest siteName serve = do
    port <- randomRIO (49152, 65535) :: IO Int
    setEnv "PORT" (show port)

    thread <- forkIO serve
    threadDelay 3000000 -- Let it start

    results <- mapM (runForConfig siteName port) configs

    killThread thread
    pure results
