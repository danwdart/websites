{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Test.E2E.Site.DanDart where

import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (unpack)
import           Site.DanDart           (serve)
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
    setEnv "PORT" "8080"
    serve
    putStrLn "Checking dandart..."
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
            ) ["Characters", "Favourites", "Ham Radio", "Health", "Music", "Maths", "About This Site", "Contact"]
        closeSession
