{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Test.E2E.Site.M0ORI where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (unpack)
import           Site.M0ORI             (serve)
import           System.Directory       (createDirectoryIfMissing)
import           System.Environment     (setEnv)
import           System.FilePath        ((<.>), (</>))
import           Test.Hspec
import           Test.WebDriver         (Selector (ByLinkText), WDConfig,
                                         chrome, click, closeSession,
                                         defaultConfig, findElem, openPage,
                                         runSession, saveScreenshot, useBrowser)
import           Test.WebDriver.Config  (WDConfig, defaultConfig, useBrowser)


-- prop_RevRev xs = reverse (reverse xs) == xs
--   where types = xs::[Int]

firefoxConfig ∷ WDConfig
firefoxConfig = defaultConfig

chromeConfig ∷ WDConfig
chromeConfig = useBrowser chrome defaultConfig

main ∷ IO ()
main = do
    serve
    putStrLn "Checking m0ori..."
    -- quickCheck prop_RevRev
    runSession chromeConfig $ do
        openPage "https://m0ori.com"
        liftIO . createDirectoryIfMissing True $ "images"
        saveScreenshot "images/home.png"
        mapM_ (\linkName -> do
            liftIO . putStrLn $ "Clicking " <> unpack linkName
            link <- findElem $ ByLinkText linkName
            click link
            liftIO . putStrLn $ "Saving " <> unpack linkName
            saveScreenshot $ "images" </> unpack linkName <.> "png"
            ) ["Ham Radio", "Contact"]
        closeSession
