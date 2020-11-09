{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.CommonSpec where

import           Control.Concurrent               (forkIO, killThread,
                                                   threadDelay)
import           Data.Aeson                       (Object,
                                                   Value (Array, String),
                                                   object)
import           Data.Text                        (unpack)
import           Data.Vector                      (fromList)
import qualified Site.Blog     as B
import qualified Site.DanDart  as D
import qualified Site.JolHarg  as J
import qualified Site.M0ORI    as M
import           System.Directory                 (createDirectoryIfMissing)
import           System.Environment               (setEnv)
import           System.FilePath                  ((<.>), (</>))
import           System.Random                    (Random (randomRIO))
import           Test.Hspec.WebDriver hiding (setWindowSize)
import           Data.List                        (nub)
import Control.Monad.IO.Class ( MonadIO(liftIO) )
import Test.WebDriver (WD, Capabilities)
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Class as W
import qualified Test.WebDriver.Commands as W hiding (setWindowSize)
import qualified Test.WebDriver.Commands.Internal as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Capabilities as W
import qualified Test.WebDriver.JSON as W
import qualified Test.WebDriver.Session as W


firefoxCap = firefoxCaps {
    W.additionalCaps = [
        ("moz:firefoxOptions", object [
            ("args", Array (fromList [String "--headless"]))
        ])
    ]
}

chromeCap = chromeCaps {
    W.browser = W.chrome {
        W.chromeOptions = ["--headless"]
    }
}

-- |Set the dimensions of the current window.
setWindowSize :: (Word, Word) -> WD Object
setWindowSize = W.doWinCommand W.methodPost currentWindow "/size"
                . W.pair ("width", "height")

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

configs = [firefoxCap, chromeCap]

runTest siteName serve = do
    port <- runIO (randomRIO (49152, 65535) :: IO Int)
    runIO $ setEnv "PORT" (show port :: String)

    thread <- runIO $ forkIO serve
    runIO $ threadDelay 3000000 -- Let it start

    parallel . session siteName . using configs $
        mapM (\winSize -> describe (showTuple winSize) $ do
            let res = showTuple winSize
            it "nav height is equal to 39" . runWD $ do
                setWindowSize winSize
                navbar <- findElem $ ByClass "navbar-nav"
                (_, navHeight) <- elemSize navbar
                navHeight `shouldBe` 39
            describe "links" . do
                links <- findElems $ ByCSS ".navbar-nav label a"
                liftIO . print $ links
                mapM (\link -> do
                    linkName <- runWD $ getText link
                    describe (unpack linkName) .
                        it "visible cards are only one size" . runWD $ do
                            linkName <- getText link
                            click link
                            cards <- findElems $ ByClass "card"
                            cardSizes <- mapM elemSize cards
                            (length . nub . filter (/= (0, 0)) $ cardSizes )`shouldSatisfy` (< 2)
                    ) links
            ) resolutions

    runIO $ killThread thread

spec = mapM_ (\(siteName, serve) ->
    describe siteName . parallel $ runTest siteName serve
    ) [
        ("blog", B.serve),
        ("dandart", D.serve),
        ("jolharg", J.serve),
        ("m0ori", M.serve)
    ]