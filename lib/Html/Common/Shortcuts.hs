{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Shortcuts where

import Html.Common.Link
import Text.Blaze.Html5 as H hiding (main)

ghPages, ghPagesProjects, projectsSource, imdb, yt, ytChan, ytUser, nhs, oeis ∷ AttributeValue
ghPages = "https://danwdart.github.io/"
ghPagesProjects = ghPages <> "projects/"
projectsSource = "https://github.com/danwdart/projects/tree/master"
ytChan = "https://www.youtube.com/channel/"
ytUser = "https://www.youtube.com/user/"
yt = "https://www.youtube.com/watch?v="
imdb = "https://www.imdb.com/title/tt"
nhs = "https://www.nhs.uk/conditions/"
oeis = "https://oeis.org/A"

wiki ∷ AttributeValue → AttributeValue → AttributeValue → AttributeValue
wiki name' subdomain article' = "https://" <> subdomain <> "." <> name' <> "/wiki/" <> article'

wikipedia ∷ AttributeValue → AttributeValue
wikipedia = wiki "wikipedia.org" "en"

wikia ∷ AttributeValue → AttributeValue → AttributeValue
wikia = wiki "wikia.com"

fandom ∷ AttributeValue → AttributeValue → AttributeValue
fandom = wiki "fandom.com"

bulbapedia ∷ AttributeValue → AttributeValue
bulbapedia pokémon = "https://bulbapedia.bulbagarden.net/wiki/" <> pokémon <> "_(Pok%C3%A9mon)"

babby ∷ Html → Html
babby = extLink "https://yanderedarling.com"
