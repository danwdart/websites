{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.BlogSpec where

import           Control.Concurrent     (forkIO, killThread)
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (unpack)
import           Site.Blog              (serve)
import           System.Directory       (createDirectoryIfMissing)
import           System.Environment     (setEnv)
import           System.FilePath        ((<.>), (</>))
import           Test.Hspec             (Spec, describe, it)
import           Test.WebDriver         (Selector (ByLinkText), WDConfig,
                                         chrome, click, closeSession,
                                         defaultConfig, findElem, openPage,
                                         runSession, saveScreenshot, useBrowser)

firefoxConfig ∷ WDConfig
firefoxConfig = defaultConfig

chromeConfig ∷ WDConfig
chromeConfig = useBrowser chrome defaultConfig

spec ∷ Spec
spec = do
    describe "Blog" .
        it "serves and checks sizes" $ do
            setEnv "PORT" "8080"
            thread <- forkIO serve
            putStrLn "Checking blog..."
            -- quickCheck prop_RevRev
            runSession chromeConfig $ do
                openPage "http://localhost:8080"
                liftIO . createDirectoryIfMissing True $ "images"
                saveScreenshot "images/home.png"
                mapM_ (\linkName -> do
                    liftIO . putStrLn $ "Clicking " <> unpack linkName
                    link <- findElem $ ByLinkText linkName
                    click link
                    liftIO . putStrLn $ "Saving " <> unpack linkName
                    saveScreenshot $ "images" </> unpack linkName <.> "png"
                    ) ["Blog"]
                closeSession
            killThread thread
