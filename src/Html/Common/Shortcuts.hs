{-# LANGUAGE OverloadedStrings #-}

module Html.Common.Shortcuts (ghPages, ghPagesProjects, projectsSource, imdb, wikipedia, yt, ytChan, ytUser, nhs, oeis, wikia, fandom) where

import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A

ghPages, ghPagesProjects, projectsSource, imdb, yt, ytChan, ytUser, nhs, oeis :: AttributeValue
ghPages = "https://danwdart.github.io/"
ghPagesProjects = ghPages <> "projects/"
projectsSource = "https://github.com/danwdart/projects/tree/master"
ytChan = "https://www.youtube.com/channel/"
ytUser = "https://www.youtube.com/user/"
yt = "https://www.youtube.com/watch?v="
imdb = "https://www.imdb.com/title/tt"
nhs = "https://www.nhs.uk/conditions/"
oeis = "https://oeis.org/A"

wiki :: AttributeValue -> AttributeValue -> AttributeValue -> AttributeValue
wiki name subdomain article = "https://" <> subdomain <> "." <> name <> "/wiki/" <> article

wikipedia :: AttributeValue -> AttributeValue
wikipedia = wiki "wikipedia.org" "en"

wikia:: AttributeValue -> AttributeValue -> AttributeValue
wikia = wiki "wikia.com"

fandom :: AttributeValue -> AttributeValue -> AttributeValue
fandom = wiki "fandom.com"
