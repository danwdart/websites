{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Base
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Text
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
    -- quickCheck prop_RevRev
    runSession chromeConfig $ do 
        openPage "https://dandart.co.uk"
        body <- findElem $ ByTag "body"
        text <- attr body "innerHTML"
        liftIO . print $ text
        saveScreenshot "bob.png"
        closeSession