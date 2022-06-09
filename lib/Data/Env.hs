{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Data.Env where

import qualified Build.Blog      as B
import qualified Build.DanDart   as D
import qualified Build.JolHarg   as J
import qualified Build.M0ORI     as M
import qualified Build.MadHacker as MH
import           Data.Env.Types

productionUrls ∷ Urls
productionUrls = Urls {
    urlDanDart = "https://dandart.co.uk",
    urlHamRadio = "https://m0ori.com",
    urlBlog = "https://blog.dandart.co.uk",
    urlJolHarg = "https://jolharg.com",
    urlMadHacker = "https://madhackerreviews.com"
}

prodBlog,
    prodDanDart,
    prodJolHarg,
    prodM0ORI,
    prodMadHacker ∷ Website
prodBlog = Website {
    slug = "blog",
    title = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio) Ham, Musician",
    url = urlBlog productionUrls,
    urls = productionUrls,
    siteType = Blog "posts",
    livereload = False,
    build = B.build
}
prodDanDart = Website {
    slug = "dandart",
    title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
    url = urlDanDart productionUrls,
    urls = productionUrls,
    siteType = Normal,
    livereload = False,
    build = D.build
}
prodJolHarg = Website {
    slug = "jolharg",
    title = "JolHarg: Your Software Engineering Partner",
    url = urlJolHarg productionUrls,
    urls = productionUrls,
    siteType = Normal,
    livereload = False,
    build = J.build
}
prodM0ORI = Website {
    slug = "m0ori",
    title = "M0ORI call sign: Dan Dart, England",
    url = urlHamRadio productionUrls,
    urls = productionUrls,
    siteType = Normal,
    livereload = False,
    build = M.build
}
prodMadHacker = Website {
    slug = "madhacker",
    title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
    url = urlMadHacker productionUrls,
    urls = productionUrls,
    siteType = Blog "reviews",
    livereload = False,
    build = MH.build
}

production ∷ Env
production = [
    prodBlog,
    prodDanDart,
    prodJolHarg,
    prodM0ORI,
    prodMadHacker
    ]
