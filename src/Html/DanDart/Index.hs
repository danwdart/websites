{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Index (page, page404) where

import           Data.Env
import           Data.Site.DanDart
import           Html.Common.Audio
import Html.Common.Bootstrap
import           Html.Common.Contact
import           Html.Common.Error.NotFound
import           Html.Common.Head
import           Html.Common.Header
import           Html.Common.Link
import           Html.Common.Page
import           Html.Common.Shortcuts
import           Html.Common.Social
import           Html.Common.Visit
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

pageIntro ∷ WebsiteM Html
pageIntro = makePage "intro" "Intro" defaultLayout defaultPage $ do
    p "Hello, my name is Dan."
    p "I am a software engineer, mathematics lover, radio ham and musician."
    p $ do
        "I work remotely to care for my future wife, "
        extLink "https://yanderedarling.com/" "Toga Himiko"
        "."
    p "I also enjoy discordant and nonsensical commentary."
    p "I can speak about maths, physics, computer science and linguistics at length."
    p "You can find out more by using the links at the top."
    br
    p $ do
        extLink "https://html.spec.whatwg.org/" $ img ! A.style "height: 16px" !  src "https://upload.wikimedia.org/wikipedia/commons/a/a1/WHATWG_logo.svg"
        extLink "/humans.txt" $ img ! src "/img/humanstxt-isolated-blank.gif"


pageCharacters ∷ WebsiteM Html
pageCharacters = makePage "characters" "Characters" defaultLayout notDefaultPage $ do
    p "Some of my favourite characters and characters that I identify with are:"
    ul $ mapM_ (\(fandom', fandomLink, characters) -> do
        "from "
        extLink fandomLink fandom'
        ":"
        ul $ mapM_ (\(character, charLink, reason) -> li $ do
                extLink charLink character
                ", because "
                reason
            ) characters
        ) favCharacters

pageFavourites ∷ WebsiteM Html
pageFavourites = makePage "favourites" "Favourites" defaultLayout notDefaultPage $ do
    p "Here is a list of some of my favourite things."
    p $ strong "YouTube channels"
    ul $ do
        li $ strong "Maths"
        ul $ do
            li $ extLink (ytUser <> "numberphile") "Numberphile"
            li $ extLink (ytChan <> "UC1_uAIS3r8Vu6JjXWvastJg") "Mathologer"
        li $ strong "Science"
        ul . li $ extLink (ytUser <> "Vsauce") "Vsauce"
        li $ strong "Computer Science"
        ul . li $ extLink (ytUser <> "Computerphile") "Computerphile"
        li $ strong "General Knowledge / Other"
        ul . li $ extLink (ytChan <> "UC9pgQfOXRsp4UKrI8q0zjXQ") "Lindybeige"
    p $ do
        strong "TV shows/movies"
        " (I also have an "
        extLink "https://www.imdb.com/list/ls029966237/" "IMDB"
        " watchlist)"
    ul $ do
        li $ do
            "The Hannibal film and TV series:"
            ul $ do
                li $ do
                    extLink (imdb <> "0091474") "Manhunter (1986)"
                    br
                    "(well, you know... it's got... Iron Butterfly? *shrug*)"
                li $ extLink (imdb <> "0102926") "The Silence of the Lambs (1991)"
                li $ extLink (imdb <> "0212985") "Hannibal (2001)"
                li $ extLink (imdb <> "0289765") "Red Dragon (2002)"
                li $ extLink (imdb <> "0367959") "Hannibal Rising (2007)"
                li $ extLink (imdb <> "2243973") "Hannibal (TV series, 2013-)"
        li $ do
            "Star Trek films and series:"
            ul $ do
                li $ extLink (imdb <> "0092007") "Star Trek IV: The Voyage Home (1986)"
                li $ extLink (imdb <> "0117731") "Star Trek: First Contact (1996)"
                li $ extLink (imdb <> "0092455") "Star Trek: The Next Generation (1987-1994)"
        li $ do
            extLink (imdb <> "0056751") "Doctor Who (1963-)"
            " (my favourite Doctor is Tom Baker)"
    p $ strong "Music"
    ul $ mapM_ (\(title', list') -> do
        title'
        ul $ mapM_ li list'
        ) musicList
    p $ strong "Musical styles"
    ul $ mapM_ li musicalStyles
    p $ strong "Games"
    ul $ do
        li $ extLink "http://www.idsoftware.com/en-gb" "Quake"
        li $ extLink "http://sauerbraten.org/" "Cube 2: Sauerbraten"
        li $ extLink (wikipedia "The_Elder_Scrolls_IV:_Oblivion") "The Elder Scrolls IV: Oblivion"
        li $ extLink "https://ddlc.moe/" "Doki Doki Literature Club"
        li $ do
            extLink "https://danganronpa.us/" "Danganronpa"
            " (no despair girls / hope side spoilers please!)"
    p $ strong "Coding language:"
    ul . li $ (do
        extLink "https://www.haskell.org/" "Haskell"
        " (it's epic and pure!)")
    p $ strong "Operating Systems"
    ul $ do
        li $ do
            extLink "http://www.gnu.org/gnu/why-gnu-linux.en.html" "GNU/Linux"
            ": "
            extLink "http://kubuntu.org" "Kubuntu"
        li $ do
            extLink (wikipedia "Blue_Screen_of_Death") "Windows"
            ": "
            extLink (wikipedia "Windows_98#Windows_98_Second_Edition") "98 SE"
        li $ do
            "All-time: "
            extLink "http://riscos.com/riscos/310/index.php" "RISC OS"

pageHealth ∷ WebsiteM Html
pageHealth = makePage "health" "Health" defaultLayout notDefaultPage $ do
    p "Both my physical and mental health are very low at the moment, but I am always more than happy to talk about them."
    p "I think I'm addicted to caffeine, which I wouldn't recommend."
    p "I have been diagnosed with the following things, both physical and mental intermingling:"
    ul $ do
        li $ extLink (nhs <> "memory-loss-amnesia/") "short-term memory loss"
        li $ extLinkTitle (nhs <> "post-traumatic-stress-disorder-ptsd") "Post-traumatic Stress Disorder" "PTSD"
        li $ extLink (nhs <> "fibromyalgia") "Fibromyalgia"
        li $ extLink (nhs <> "autism") "Asperger's Syndrome"
        li $ extLink (nhs <> "attention-deficit-hyperactivity-disorder-adhd") "ADHD"
        li $ do
            extLink (nhs <> "generalised-anxiety-disorder") "Anxiety"
            " (with depression)"

pageMusic ∷ WebsiteM Html
pageMusic = makePage "music" "Music" defaultLayout notDefaultPage $ do
    p "I play the guitar, keyboard and synthesiser."
    p "I've created the following pieces of music/sound effects:"
    audioFile "Gothic Orchestra" "GothicOrchestra" "SatanicOrchestra"
    audioFile "Shall It Be" "ShallItBe" "ShallItBe"
    audioFile "Swim Deep (take 1)" "SwimDeepTake1" "SwimDeepTake1"

pageMaths ∷ WebsiteM Html
pageMaths = makePage "maths" "Maths" defaultLayout notDefaultPage $ do
    p "Mathematics has always been a great pastime for me."
    p $ do
        "I have invented quite a few visualisations and generators for several interesting pieces of mathematics, some of which you can see and try on repos like my projects repo on GitHub ("
        extLink (projectsSource <> "/haskell/maths/src") "Haskell examples"
        ", "
        extLink (projectsSource <> "/js/maths") "JS examples"
        ")"
    p "Some web-based notable (read: working right now) examples are:"
    ul $ do
        li $ extLink (ghPagesProjects <> "js/maths/src/cfe/index.html") "Continued Fraction Expander"
        li $ do
            "A set of gravity simulators:"
            ul $ do
                li $ extLink (ghPages <> "gravity/2/gravity.html") "Sun version (left click for planets, middle click for stars)"
                li $ extLink (ghPages <> "gravity/4/gravity.html") "Black hole version"
        li $ extLink (ghPagesProjects <> "js/maths/src/cellautomata/") "Cell Automata"
        li $ do
            extLink (ghPagesProjects <> "js/maths/src/graphAndSound/") "Graph and Sound Demos"
            br
            "(one of these is actually the answer similar to "
            extLink (yt <> "zSL8asTgpzA") "the riddle"
            " I posed on my YouTube channel)"
        li $ extLink (ghPagesProjects <> "js/rolling/") "Rolling Shutter effect example"
        li $ extLink (ghPages <> "heartish/cardint.html") "Interactive Cardioid (keyboard only)"
        li $ extLink (ghPages <> "heartish/card.html") "Random Cardioid"
        li $ extLink (ghPages <> "heartish/index.html") "Circle Reflection"
    p $ do
        "My approved "
        extLinkTitle "https://oeis.org" "Online Encyclopedia of Integer Sequences" "OEIS"
        " sequences are:"
    ul $ do
        li $ extLink (oeis <> "275124") "A275124: Multiples of 5 where Pisano periods of Fibonacci numbers A001175 and Lucas numbers A106291 agree."
        li $ extLink (oeis <> "275167") "A275167: Pisano periods of A275124."
        li $ extLink (oeis <> "308267") "A308267: Numbers which divide their Zeckendorffian format exactly."
        li $ extLink (oeis <> "309979") "A309979: Hash Parker numbers: Integers whose real 32nd root's first six nonzero digits (after the decimal point) rearranged in ascending order are equal to 234477."

pageOrigami ∷ WebsiteM Html
pageOrigami = makePage "origami" "Origami" defaultLayout notDefaultPage $ do
    p "I've been doing origami from a very young age. I will give some instructions on how to make some models that I've invented later on when I've figured out how to digitise them, but for now, I'll give you some of my favourite origami resources:"
    br
    p $ do
        extLink "https://amzn.to/2BWtHhY" "Complete Origami, a book by the late Eric Kenneway"
        br
        extLink "https://amzn.to/2BqLCO4" "Star Trek Paper Universe, a book by Andrew Pang"
        br
        extLink "https://amzn.to/2BspLWn" "Ultimate Origami Kit: The Complete Step-by-step Guide to the Art of Paper Folding, a book by John Morin"
        br
        extLink "https://amzn.to/2VzLzqe" "How to Make Origami Airplanes That Fly, a book by Gery Hsu"

pageAbout ∷ WebsiteM Html
pageAbout = makePage "about" "About" defaultLayout notDefaultPage $ do
    p "This website entailed a few design and code decisions which I would like to explain."
    p mempty
    p $ do
        strong "The layout"
        " was based on Bootstrap. I kept the header component and chose to be without a footer component. The menus are actually a hack, such that the chosen menu item is linked to a hidden radio button which chose which sub-page to show, rather than using JS for the menu."
    p mempty
    p $ do
        strong "The code"
        " actually contains no client-side JS at all, therefore, also adding to the preference of more and more users these days to not have tracking. The website code is compiled using a custom Haskell-based codebase based on Blaze, and uploaded to GitHub Pages."
    p mempty
    p $ do
        strong "The font choice"
        " was difficult to make, as I was (and am still not quite happy enough with it, and so therefore still am) looking for a suitable, free software natural sans-serif font style, which has the single-storey \"a\", non-looped \"g\", and the double-seriffed I and J amongst other things. For now I've settled on Caudex, which whilst it is still serif, seems to be the closest I've yet to come across."

pageHamRadio ∷ WebsiteM Html
pageHamRadio = do
    urlHamRadio' <- asks urlHamRadio
    pure $ extNav (textValue urlHamRadio') "Ham Radio"

pageSoftware ∷ WebsiteM Html
pageSoftware = do
    urlJolHarg' <- asks urlJolHarg
    pure $ extNav (textValue urlJolHarg') "Software"

pageBlog ∷ WebsiteM Html
pageBlog = do
    urlBlog' <- asks urlBlog
    pure $ extNav (textValue urlBlog') "Blog"

pageReviews ∷ WebsiteM Html
pageReviews = do
    urlMadHacker' <- asks urlMadHacker
    pure $ extNav (textValue urlMadHacker') "Reviews"

pageContact ∷ WebsiteM Html
pageContact = do
    contactForm' <- contactForm "website@dandart.co.uk" emailHelpSingular "Greetings..." "Hello!..."
    makePage "contact" "Contact" contactLayout notDefaultPage $ do
        contactForm'

socialIcons ∷ Html
socialIcons = (divClass "row social-row") . (divClass "text-right social-inside") $ (do
    -- socialIconBBanned "" "No Blogger" "blogger-b"
    socialIconB "https://joindiaspora.com/people/08b11e5f4fff2a8b" "Diaspora" "diaspora"
    -- +social('discord', 'Discord', 'url', 'black')
    -- socialIconBBanned "" "No Facebook" "facebook-f"
    --  +social('firefox
    socialIconS "mailto:website@dandart.co.uk" "Email" "envelope"
    socialIconB "https://github.com/danwdart" "GitHub" "github"
    -- socialIconBBanned "" "No Google" "google"
    socialIconB "https://news.ycombinator.com/user?id=dandart" "Hacker News" "hacker-news"
    socialIconB "https://www.hackerrank.com/dandart" "HackerRank" "hackerrank"
    socialIconB "https://www.imdb.com/user/ur81806610" "ImDB" "imdb"
    -- socialIconBBanned "" "No Instagram" "instagram"
    -- +social('keybase', 'Keybase', 'url', 'black')
    socialIconB "https://www.last.fm/user/DanDart" "Last.fm" "lastfm"
    socialIconB "https://www.linkedin.com/in/dandart" "LinkedIn" "linkedin"
    -- +social('linux', 'Linux', 'url', 'black')
    -- +social('microsoft', 'Microsoft', 'url', 'black')
    socialIconB "https://mix.com/dandart" "Mix" "mix"
    socialIconB "https://www.npmjs.com/~dandart" "npm" "npm"
    socialIconB "https://www.patreon.com/danwdart" "Patreon" "patreon"
    socialIconB "https://my.playstation.com/profile/MeowzorFnord" "PlayStation" "playstation"
    -- socialIconBBanned "" "No Pinterest" "pinterest"
    -- +social('raspberry-pi', 'Raspberry PI', 'url', 'black')
    socialIconB "https://www.reddit.com/user/jolharg" "Reddit" "reddit"
    socialIconB "skypeurl" "Skype" "skype"
    -- socialIconBBanned "" "No Snapchat" "snapchat"
    socialIconB "https://soundcloud.com/danwdart" "SoundCloud" "soundcloud"
    socialIconB "https://open.spotify.com/user/dandart" "Spotify" "spotify"
    socialIconB "https://stackoverflow.com/users/1764563/dan-dart" "Stack Overflow" "stack-overflow"
    socialIconB "https://steamcommunity.com/id/dandart" "Steam" "steam"
    socialIconB "https://yanderehiro.tumblr.com/" "Tumblr" "tumblr"
    -- socialIconBBanned "" "No Twitter" "twitter"
    -- +social('ubuntu', 'Ubuntu', 'url', 'black')
    -- +social-no('windows', 'Windows', 'url', 'black')
    socialIconB (ytChan <> "UCaHwNzu1IlQKWCQEXACflaw") "YouTube" "youtube")

htmlHeader ∷ WebsiteM Html
htmlHeader = do
    pages <- do
        pageIntro <>
            pageCharacters <>
            pageFavourites <>
            pageHamRadio <>
            pageHealth <>
            pageMusic <>
            pageMaths <>
            pageOrigami <>
            pageAbout <>
            pageSoftware <>
            pageBlog <>
            pageReviews <>
            pageContact
    pure . makeHeader "#intro" "Dan Dart" socialIcons $ pages

page ∷ WebsiteM Html
page = do
    header' <- htmlHeader
    head' <- htmlHead descTitle keywords mempty
    visit' <- visit "dandart"
    pure $ do
        docTypeHtml ! lang "en-GB" $ do
            head'
            header'
            visit'

page404 ∷ WebsiteM Html
page404 = do
    visit' <- visit "dandart404"
    defaultPage404 descTitle keywords visit'
