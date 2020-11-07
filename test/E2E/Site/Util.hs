{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.Util where

import           Control.Concurrent               (threadDelay, forkIO, killThread)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.Aeson                       (Object)
import           Data.Text                        (Text, unpack)
import           System.Directory                 (createDirectoryIfMissing)
import           System.Environment               (setEnv)
import           System.FilePath                  ((<.>), (</>))
import           System.Random
import           Test.Hspec                       (HasCallStack)
import           Test.WebDriver                   (Browser (chromeOptions),
                                                   Capabilities (browser),
                                                   Selector (ByLinkText), WD,
                                                   WDConfig (wdCapabilities),
                                                   chrome, click, closeSession,
                                                   currentWindow, defaultCaps,
                                                   defaultConfig, findElem,
                                                   openPage, runSession,
                                                   saveScreenshot)
import           Test.WebDriver.Class             (WebDriver, methodPost)
import           Test.WebDriver.Commands.Internal (doWinCommand)
import           Test.WebDriver.JSON              (pair)

firefoxConfig ∷ WDConfig
firefoxConfig = defaultConfig

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

resolutions ∷ [(Word, Word)]
resolutions = [
    (360, 480),
    (480, 360),
    (480, 720),
    (720, 480),
    (1024, 768),
    (768, 1024),
    (1280, 720),
    (720, 1280),
    (1440, 900),
    (900, 1440),
    (1920, 1080),
    (1080, 1920)
    ]

runTest ∷ Foldable t ⇒ FilePath → t Text → IO () -> IO ()
runTest name links serve = do
    port <- randomRIO (25915, 65535) :: IO Int
    setEnv "PORT" (show port)
    thread <- forkIO serve
    threadDelay 2000000 -- Let it start
    putStrLn $ "Checking " <> name <> "..."
    runSession chromeConfig $ do
        openPage $ "http://" <> name <> ".localhost:" <> show port
        liftIO . createDirectoryIfMissing True $ "images/" <> name
        mapM_ (\winSize -> (do
            let res = show (fst winSize) <> "x" <> show (snd winSize)
            _ <- setWindowSize winSize
            saveScreenshot $ "images/" <> name <> "/home-" <> res <> ".png"
            mapM_ (\linkName -> do
                let fileName = unpack linkName <> "-" <> res
                liftIO . putStrLn $ "Clicking " <> fileName
                link <- findElem $ ByLinkText linkName
                click link
                liftIO . putStrLn $ "Saving " <> fileName
                saveScreenshot $ "images" </> name </> fileName <.> "png"
                ) links)
                :: WD ()) resolutions
        closeSession
    killThread thread
