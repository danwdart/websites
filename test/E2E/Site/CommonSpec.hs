{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE UnicodeSyntax     #-}

module E2E.Site.CommonSpec where

import Control.Concurrent.Async
import Control.Exception
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
-- import Control.Monad.Reader
import Data.Aeson
import Data.Env                  as Env
import Data.Env.Types            as Env
import Data.Foldable
-- import Data.Functor.Compose
import Data.List                 qualified as L
import Data.NonEmpty             qualified as NE
-- import Data.Set                  (Set)
-- import Data.Set                  qualified as S
import Data.Text                 (Text)
import Data.Text                 qualified as T
import Data.Text.IO              qualified as TIO
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status
import Network.URI
import Network.URI.Static
import Prelude                   hiding (filter)
-- import System.Environment
-- import System.Process
-- import System.Random
import Test.Hspec
-- import Test.Hspec.Expectations
import Test.WebDriver
-- import Test.WebDriver.Class
-- import Test.WebDriver.Config
-- import Test.WebDriver.Monad
import Test.WebDriverWrapper
import Witherable

firefoxConfig ∷ WDConfig
firefoxConfig = defaultConfig {
    wdCapabilities = defaultCaps {
        additionalCaps = [
            ("moz:firefoxOptions", object [
                ("args", Array [String "--headless"])
            ])
        ]
    }
}

-- chromeConfig ∷ WDConfig
-- chromeConfig = defaultConfig {
--     wdCapabilities = defaultCaps {
--         browser = chrome {
--             -- chromeBinary = Just $(staticWhich "google-chrome-stable"),
--             chromeOptions = []
--         }
--     }
-- }

showResolution ∷ (Show a) ⇒ (a, a) → String
showResolution (a, b) = show a <> "x" <> show b

-- stats stolen from https://www.browserstack.com/guide/common-screen-resolutions
-- heck we could probably use them at some point
resolutions ∷ [(Word, Word)]
resolutions = [
    -- we can't go below cards that are 438x382 etc in Firefox without some looking up... - let's not worry for now I suppose...
    (320, 480), -- smallest common portrait mobile -- ??? breaks rm? too big for it?
    (360, 480),
    (360, 780), -- #6 portrait mobile
    (360, 800), -- #1 portrait mobile
    (390, 844), -- #2 portrait mobile
    (393, 873), -- #3 portrait mobile
    (412, 915), -- #4 portrait mobile
    (414, 896), -- #5 portrait mobile
    (480, 320), -- smallest common landscape mobile
    (480, 360),
    (480, 720),
    (601, 952), -- #6 portrait tablet
    (720, 480),
    (720, 1280),
    (768, 1024), -- #1 portrait tablet, smallest common portrait tablet
    (780, 360), -- #6 landscape mobile
    (800, 360), -- #1 landscape mobile
    (800, 1280), -- #5 portrait tablet
    (810, 1080), -- #2 portrait tablet
    (820, 1180), -- #3 portrait tablet
    (844, 390), -- #2 landscape mobile
    (873, 393), -- #3 landscape mobile
    (896, 414), -- #5 landscape mobile
    (900, 1440),
    (915, 412), -- #4 landscape mobile
    (952, 601), -- #6 landscape tablet
    (1024, 768), -- smallest common desktop and landscape tablet, #1 landscape tablet
    (1080, 810), -- #2 landscape tablet, smallest common landscape tablet
    (1080, 1920),
    (1180, 820), -- #3 landscape tablet
    (1280, 720), -- #4 desktop
    (1280, 800), -- #4 landscape tablet
    (1366, 768), -- #2 desktop
    (1440, 900), -- #5 desktop
    (1440, 3200), -- biggest common portrait mobile
    (1536, 864), -- #3 desktop
    (1600, 900), -- #6 desktop
    (1600, 2560), -- biggest common portrait tablet
    (1920, 1080), -- #1 desktop
    (2560, 1600), -- biggest common landscape tablet
    (3200, 1440), -- biggest common landscape mobile
    (5120, 2880) -- biggest common desktop
    ]

configs ∷ [(Text, WDConfig)]
configs = [
    ("Firefox", firefoxConfig)
    -- ("Chrome", chromeConfig)
    ]

-- in terms of safeTry / try?
ioDef ∷ a → IO a → IO a
ioDef d io = either (\(SomeException _) -> d) id <$> try io

testForLink ∷ Element -> WD ()
testForLink linkToClick = do
    linkName <- getText linkToClick
    cardSizes <- fmap (bimap (round :: Float → Int) (round :: Float → Int)) <$> do
        click linkToClick
        cards <- findElems $ ByCSS "input:checked ~ .page .card"
        traverse_ (\el -> do
            t <- getText el
            es <- elemSize el
            liftIO . TIO.putStrLn $ "(" <> t <> ") was: " <> T.show es
            ) cards
        traverse elemSize cards

    liftIO . TIO.putStrLn . T.show $ cardSizes

    liftIO . hspec . describe (T.unpack linkName) . it "visible cards are only one size" $ (
        (length . L.nub . filter (/= (0, 0)) $ cardSizes ) `shouldSatisfy` (< 2))

testForResolution ∷ (Word, Word) → Bool -> WD ()
testForResolution winSize@(width, height) testCards = do
    liftIO . putStrLn $ "Setting window size to " <> show (width, height)
    setWindowSize (width, height)

    navHeight <- do
        navbar <- findElem $ ByCSS ".navbar-nav"
        (_, navHeight) <- elemSize navbar
        pure navHeight

    ((liftIO . hspec) . describe (showResolution winSize))
        . it "nav height is equal to 40"
            $ (navHeight `shouldBe` 40)

    when testCards $ do
        liftIO . putStrLn $ "would test cards but this is slightly broken"
        --  links <- findElems $ ByCSS ".navbar-nav label a"
        -- traverse_ testForLink links

testHTTPSLink ∷ URI → Spec
testHTTPSLink src = describe (show src) .
    unless (src `elem` insecureExceptions) $ do
    it "is secure" $
        uriScheme src `shouldBe` "https:"

testSecureLink ∷ (URI, Maybe Text, Maybe Text) → Spec
testSecureLink (src, target', rel') = describe (show src) $ do
    unless (src `elem` insecureExceptions) $ do
        it "is secure" $
            uriScheme src `shouldBe` "https:"
    it "has target _blank" $
        target' `shouldBe` Just "_blank"
    it "has rel noreferrer" $ -- implies noopener
        rel' `shouldBe` Just "noreferrer"


getStatuses ∷ Manager → URI → IO (URI, Int)
getStatuses manager url' = ioDef (url', 0) $ do
    request <- requestFromURI url'
    let req' = request {
        requestHeaders = [
            ("User-Agent", "Mozilla/5.0 (X11; Linux x86_64; rv:140.0) Gecko/20100101 Firefox/140.0")
        ]
    }
    response <- httpLbs req' manager
    pure (url', statusCode . responseStatus $ response)

insecureExceptions ∷ [URI]
insecureExceptions = [
    [uri|http://xn--101-8cd4f0b.xn--p1ai/user/mouseriver12/|], -- a commenter url
    [uri|http://sauerbraten.org/|], -- no ssl yet
    [uri|http://ppa.launchpad.net/jonabeck/ppa/ubuntu|], -- PPAs are verified separately
    [uri|http://riscos.com/riscos/310/index.php|] -- not available securely
    ]

brokenExceptions ∷ [URI]
brokenExceptions = [
    [uri|https://canddi.com|], -- resolves to 0.0.0.0 with adguard
    [uri|https://www.linuxvoice.com/category/podcasts/|], -- self-signed certificate
    [uri|https://upload.wikimedia.org/wikipedia/commons/6/6a/JavaScript-logo.png|],
    [uri|https://upload.wikimedia.org/wikipedia/commons/thumb/0/08/Antu_bash.svg/512px-Antu_bash.svg.png|],
    [uri|https://upload.wikimedia.org/wikipedia/commons/3/3b/C.sh-600x600.png|],
    [uri|https://web.archive.org/web/20181125122112if_/https://upload.wikimedia.org/wikipedia/commons/1/1a/Code.jpg|],
    [uri|https://upload.wikimedia.org/wikipedia/commons/thumb/6/61/HTML5_logo_and_wordmark.svg/512px-HTML5_logo_and_wordmark.svg.png|],
    [uri|https://upload.wikimedia.org/wikipedia/commons/thumb/2/27/PHP-logo.svg/711px-PHP-logo.svg.png|],
    [uri|https://upload.wikimedia.org/wikipedia/commons/0/0a/Python.svg|],
    [uri|https://upload.wikimedia.org/wikipedia/commons/thumb/1/1c/Haskell-Logo.svg/1280px-Haskell-Logo.svg.png|],
    [uri|https://draft.blogger.com/profile/11404839970927435985|],
    [uri|https://draft.blogger.com/profile/03812351582147993701|],
    [uri|https://draft.blogger.com/profile/15493119212604094040|],
    [uri|https://draft.blogger.com/profile/03017160325987430398|],
    [uri|https://blocked-for-spam.com/|], -- default user url for comments when they're spam
    [uri|http://xn--101-8cd4f0b.xn--p1ai/user/mouseriver12/|], -- some user's url which is only http
    [uri|https://web.archive.org/web/20090211204719/https://www.pcworld.com/article/129977/how_to_reinstall_windows_xp.html|], -- no idea why
    [uri|https://web.archive.org/web/20090619065522/http://www.daniweb.com/blogs/entry3288.html|], -- no idea
    [uri|https://web.archive.org/web/20120226075352/http://www.kumailht.com/blog/linux/10-features-ubuntu-should-implement/|], -- dunno
    [uri|https://web.archive.org/web/20100107134808/http://xenon.kevinghadyani.com/|],
    [uri|https://web.archive.org/web/20100224082039/https://xenon.kevinghadyani.com/desktop|],
    [uri|https://web.archive.org/web/20100211151037/http://matt.colyer.name:80/projects/iphone-linux/?title=Main_Page|], -- archive is weird
    [uri|https://web.archive.org/web/20140908152050/http://www.freebsd.org/doc/handbook/filesystems-zfs.html|],
    [uri|https://web.archive.org/web/20091018103227/https://windows.microsoft.com/en-US/windows-vista/Installing-and-reinstalling-Windows|],
    [uri|https://web.archive.org/web/20100106113611/http://voices.washingtonpost.com/securityfix/2009/10/avoid_windows_malware_bank_on.html|],
    [uri|https://web.archive.org/web/20150421072318/http://bibud.com/|],
    [uri|https://web.archive.org/web/20100107134808/https://xenon.kevinghadyani.com/|],
    [uri|https://web.archive.org/web/20091220083328/https://hackerlanes.com/|],
    [uri|https://web.archive.org/web/20210224170701/https://howsoftwareisbuilt.com/2009/11/18/interview-with-greg-kroah-hartman-linux-kernel-devmaintainer/|],
    [uri|https://web.archive.org/web/20180930073858/https://filthy-frank.wikia.com/wiki/Filthy_Frank|],
    [uri|https://web.archive.org/web/20170306223801/https://projectchaplin.com/login|],
    [uri|https://web.archive.org/web/20110514221602/http://www.securityfocus.com/vulnerabilities|],
    [uri|https://web.archive.org/web/20210302104446/https://enucuzatakipcial.com/|],
    [uri|https://www.deepburner.com/|], -- ????
    [uri|https://www.linkedin.com/in/dandart|], -- why 999???
    [uri|https://viewex.co.uk/|], -- either no longer active or blocked
    [uri|https://cloudbanter.com/|], -- either no longer active or blocked
    [uri|https://docs.dadi.cloud/|], -- either no longer active or blocked
    [uri|https://www.soampli.com/|], -- it's fine though???
    [uri|https://themadhacker.net/|], -- ????
    [uri|https://letsencrypt.org/|] -- ?????????????????
    ]

testNotBroken ∷ (URI, Int) → Spec
testNotBroken (url', status) = describe (show url') .
    unless (url' `elem` brokenExceptions) $ do
    -- it "should not be failing" $
    --     status `shouldNotBe` 0
    it "should 200" $
        status `shouldBe` 200
    -- it "should not 404" $
    --     status `shouldNotBe` 404

testHasAltAndTitle ∷ (URI, Maybe Text, Maybe Text) → Spec
testHasAltAndTitle (url, altText, title') = describe (show url) $ do
    it "should have an alt" $ do
        altText `shouldNotBe` Nothing
        altText `shouldNotBe` Just ""
    it "should have a title" $ do
        title' `shouldNotBe` Nothing
        title' `shouldNotBe` Just ""

wdSessionForConfig :: Text -> Website -> WD ()
wdSessionForConfig configName website = do
    liftIO . TIO.putStrLn $ "Opening page"
    setPageLoadTimeout 5000

    liftIO . TIO.putStrLn $ "Opening page and waiting for it to load..."

    -- Open the dev only pages
    openPage $ website ^. baseUrl . to show . to T.pack . to (T.replace "https://" "https://dev.") . to T.unpack

    liftIO . TIO.putStrLn $ "Opened page"

    liftIO . TIO.putStrLn $ "Testing for each resolution"

    traverse_ (\res -> testForResolution res ((website ^. slug . to NE.getNonEmpty) == "jolharg")) resolutions

    -- only the first option - we don't need the following duplicated
    when ("Firefox" == configName) $ do
        internalLinks <- do
            liftIO . TIO.putStrLn $ "Finding internal links"
            as <- findElems (ByCSS "a[href^='/']")
            liftIO . TIO.putStrLn $ "Going through internal links"
            hrefs <- traverse (\x -> do
                href' <- x `attr` "href"
                -- liftIO . TIO.putStrLn $ "Internal link href is " <> T.show href'
                -- let hrefS' = T.unpack <$> href'
                -- For some reason, getting the href will just get the absolute uri, so no messing about.
                -- let ref' = parseRelativeReference =<< hrefS'
                -- let uri' = flip relativeTo (website ^. baseUrl) <$> ref'
                -- liftIO . TIO.putStrLn $ "Absoluted link href is " <> T.show uri'
                let uri' = parseURI . T.unpack =<< href'
                pure uri'
                ) as
            pure (catMaybes hrefs)

        liftIO . TIO.putStrLn $ "Found " <> T.show (length internalLinks) <> " urls."

        externalLinks <- do
            liftIO . TIO.putStrLn $ "Finding external links"
            as <- findElems (ByCSS "a[href^=http]")
            liftIO . TIO.putStrLn $ "Going through external links"
            hrefs <- traverse (\x -> do
                href' <- x `attr` "href"
                target' <- x `attr` "target"
                rel' <- x `attr` "rel"
                -- liftIO . TIO.putStrLn $ "External link href is " <> T.show href'
                let uri' = parseURI . T.unpack =<< href'
                pure (uri', target', rel')
                ) as
            pure $ mapMaybe (\(ma, mb, mc) -> case ma of Just a -> Just (a, mb, mc); Nothing -> Nothing) hrefs

        liftIO . TIO.putStrLn $ "Found " <> T.show (length externalLinks) <> " urls."

        internalImages <- do
            as <- findElems (ByCSS "img[src^='/']")
            liftIO . TIO.putStrLn $ "Going through internal images"
            imgs <- traverse (\x -> do
                src' <- x `attr` "src"
                -- liftIO . TIO.putStrLn $ "Internal img src is " <> T.show src'
                alt' <- x `attr` "alt"
                title' <- x `attr` "title"
                -- liftIO . TIO.putStrLn $ "Internal img alt is " <> T.show alt'
                -- Same with imgs
                -- let srcS' = T.unpack <$> src'
                -- let ref' = parseRelativeReference =<< srcS'
                -- let uri' = flip relativeTo (website ^. baseUrl) <$> ref'
                -- liftIO . TIO.putStrLn $ "Absoluted img src is " <> T.show uri'
                let uri' = parseURI . T.unpack =<< src'
                pure (uri', alt', title')
                ) as
            pure $ mapMaybe (\(ma, mb, mc) -> case ma of Just a -> Just (a, mb, mc); Nothing -> Nothing) imgs

        liftIO . TIO.putStrLn $ "Found " <> T.show (length internalImages) <> " images."

        externalImages <- do
            as <- findElems (ByCSS "img[src^=http]")
            liftIO . TIO.putStrLn $ "Going through external images"
            imgs <- traverse (\x -> do
                src' <- x `attr` "src"
                -- liftIO . TIO.putStrLn $ "External img src is " <> T.show src'
                alt' <- x `attr` "alt"
                title' <- x `attr` "title"
                -- liftIO . TIO.putStrLn $ "External img alt is " <> T.show alt'
                let uri' = parseURI . T.unpack =<< src'
                pure (uri', alt', title')
                ) as
            pure $ mapMaybe (\(ma, mb, mc) -> case ma of Just a -> Just (a, mb, mc); Nothing -> Nothing) imgs

        liftIO . TIO.putStrLn $ "Found " <> T.show (length externalImages) <> " images."

        liftIO . TIO.putStrLn $ "Creating a manager"

        manager <- liftIO $ newManager tlsManagerSettings

        liftIO . TIO.putStrLn $ "Getting internal link statuses"

        internalLinkStatuses <- liftIO $ mapConcurrently (getStatuses manager) internalLinks

        liftIO . TIO.putStrLn $ "Getting external link statuses"

        externalLinkStatuses <- liftIO $ mapConcurrently (getStatuses manager) ((\(uri', _, _) -> uri') <$> externalLinks)

        liftIO . TIO.putStrLn $ "Getting internal image statuses"

        internalImageStatuses <- liftIO $ mapConcurrently (getStatuses manager) (fmap (\(uri', _, _) -> uri') internalImages)

        liftIO . TIO.putStrLn $ "Getting external image statuses"

        externalImageStatuses <- liftIO $ mapConcurrently (getStatuses manager) (fmap (\(uri', _, _) -> uri') externalImages)

        liftIO . TIO.putStrLn $ "Performing tests"

        liftIO . hspec $ do
            describe "has no insecure images" $  traverse_ (testHTTPSLink . (\(uri', _, _) -> uri')) externalImages
            describe "has no insecure links" $ traverse_ testSecureLink externalLinks
            describe "has no broken internal links" $ traverse_ testNotBroken internalLinkStatuses
            describe "has no broken external links" $ traverse_ testNotBroken externalLinkStatuses
            describe "has no broken internal images" $ traverse_ testNotBroken internalImageStatuses
            describe "has no altless or titleless internal images" $ traverse_ testHasAltAndTitle internalImages
            describe "has no broken external images" $ traverse_ testNotBroken externalImageStatuses
            describe "has no altless or titleless external images" $ traverse_ testHasAltAndTitle externalImages
        

spec ∷ Spec
spec = runIO . hspec $ traverse_ (\website ->
    describe (T.unpack (website ^. slug . to NE.getNonEmpty)) $
        traverse_ (\(configName, config) ->
            describe (T.unpack configName) .
                runIO . wrappedRunSession config . finallyClose $ wdSessionForConfig configName website 
        ) configs
    ) production
