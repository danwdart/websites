{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.Util where

import           Control.Concurrent               (forkIO, killThread,
                                                   threadDelay)
import           Control.Monad.IO.Class           (MonadIO (liftIO))
import           Data.Aeson                       (Object)
import           Data.Text                        (unpack)
import           System.Directory                 (createDirectoryIfMissing)
import           System.Environment               (setEnv)
import           System.FilePath                  ((<.>), (</>))
import           System.Random                    (Random (randomRIO))
import           Test.Hspec                       (HasCallStack)
import           Test.WebDriver                   (Browser (chromeOptions),
                                                   Capabilities (browser),
                                                   Selector (ByCSS), WD,
                                                   WDConfig (wdCapabilities),
                                                   chrome, click, closeSession,
                                                   currentWindow, defaultCaps,
                                                   defaultConfig, findElems,
                                                   getText, openPage,
                                                   runSession, saveScreenshot)
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

runTest ∷ FilePath → IO () → IO ()
runTest name serve = do
    port <- randomRIO (25915, 65535) :: IO Int
    setEnv "PORT" (show port)
    putStrLn $ "Serving " <> name <> " on port " <> show port
    thread <- forkIO serve
    putStrLn "Waiting for server to start"
    threadDelay 3000000 -- Let it start
    putStrLn $ "Checking " <> name <> "..."
    runSession chromeConfig $ do
        openPage $ "http://" <> name <> ".localhost:" <> show port
        liftIO . createDirectoryIfMissing True $ "images/" <> name
        mapM_ (\winSize -> (do
            let res = show (fst winSize) <> "x" <> show (snd winSize)
            liftIO . putStrLn $ "Setting resolution to " <> res
            _ <- setWindowSize winSize
            saveScreenshot $ "images/" <> name <> "/home-" <> res <> ".png"
            links <- findElems $ ByCSS ".navbar-nav label a"
            mapM_ (\link -> do
                linkName <- getText link
                let fileName = unpack linkName <> "-" <> res
                liftIO . putStrLn $ "Clicking " <> unpack linkName
                click link
                liftIO . putStrLn $ "Saving screenshot of " <> fileName
                saveScreenshot $ "images" </> name </> fileName <.> "png"
                ) links)
                :: WD ()) resolutions
        liftIO . putStrLn $ "Closing session"
        closeSession
    putStrLn "Killing server"
    killThread thread
