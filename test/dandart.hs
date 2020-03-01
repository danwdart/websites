{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.Default
import Data.Text
import System.FilePath
import Test.QuickCheck
import Test.WebDriver
import Test.WebDriver.Config
import Test.WebDriver.Session

-- prop_RevRev xs = reverse (reverse xs) == xs
--   where types = xs::[Int]

firefoxConfig :: WDConfig
firefoxConfig = defaultConfig

chromeConfig = useBrowser chrome defaultConfig

main = do
    putStrLn "Checking dandart.co.uk..."
    -- quickCheck prop_RevRev
    runSession chromeConfig $ do 
        openPage "https://dandart.co.uk"
        saveScreenshot "images/home.png"
        mapM_ (\linkName -> do
            liftIO . putStrLn $ "Clicking " ++ unpack linkName
            link <- findElem $ ByLinkText linkName
            click link
            liftIO . putStrLn $ "Saving " ++ unpack linkName
            saveScreenshot $ "images" </> unpack linkName <.> "png"
            ) ["Characters", "Favourites", "Ham Radio", "Health", "Music", "Maths", "About This Site", "Contact"]
        -- saveScreenshot "bob.png"
        closeSession