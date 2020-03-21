{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Aeson
import Data.Default
import Data.Text
import System.Directory
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

main :: IO ()
main = do
    putStrLn "Checking jolharg.com..."
    -- quickCheck prop_RevRev
    runSession chromeConfig $ do 
        openPage "https://jolharg.com"
        liftIO . createDirectoryIfMissing True $ "images"
        saveScreenshot "images/home.png"
        mapM_ (\linkName -> do
            liftIO . putStrLn $ "Clicking " ++ unpack linkName
            link <- findElem $ ByLinkText linkName
            click link
            liftIO . putStrLn $ "Saving " ++ unpack linkName
            saveScreenshot $ "images" </> unpack linkName <.> "png"
            ) ["Free Software", "Contact"]
        closeSession