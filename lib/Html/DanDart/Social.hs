{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Html.DanDart.Social where

import           Html.Common.Shortcuts
import           Html.Common.Social
import           Text.Blaze.Html5            as H hiding (main)
import           Text.Blaze.Html5.Attributes as A

socialIcons ∷ Html
socialIcons = (H.div ! class_ "row social-row") . (H.div ! class_ "text-end social-inside") $ (do
    socialIconS "mailto:website@dandart.co.uk" "Email" "envelope"
    socialIconB "https://github.com/danwdart" "GitHub" "github"
    socialIconB "https://www.imdb.com/user/ur81806610" "ImDB" "imdb"
    socialIconB "https://www.last.fm/user/DanDart" "Last.fm" "lastfm"
    socialIconB "https://www.linkedin.com/in/dandart" "LinkedIn" "linkedin"
    socialIconB "https://www.npmjs.com/~dandart" "npm" "npm"
    socialIconB "https://www.reddit.com/user/jolharg" "Reddit" "reddit"
    socialIconB "skype:dandart?userinfo" "Skype" "skype"
    socialIconB "https://soundcloud.com/kathiedart" "SoundCloud" "soundcloud"
    socialIconB "https://open.spotify.com/user/dandart" "Spotify" "spotify"
    socialIconB "https://stackoverflow.com/users/1764563/dan-dart" "Stack Overflow" "stack-overflow"
    socialIconB "https://steamcommunity.com/id/dandart" "Steam" "steam"
    socialIconB "https://yanderehiro.tumblr.com/" "Tumblr" "tumblr"
    socialIconB (ytChan <> "UCaHwNzu1IlQKWCQEXACflaw") "YouTube" "youtube")
