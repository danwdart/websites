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

developmentUrls, productionUrls ∷ Urls
developmentUrls = Urls {
    urlDanDart = "http://dandart.localhost:8080",
    urlHamRadio = "http://m0ori.localhost:8080",
    urlBlog = "http://blog.localhost:8080",
    urlJolHarg = "http://jolharg.localhost:8080",
    urlMadHacker = "http://madhacker.localhost:8080"
}

productionUrls = Urls {
    urlDanDart = "https://dandart.co.uk",
    urlHamRadio = "https://m0ori.com",
    urlBlog = "https://blog.dandart.co.uk",
    urlJolHarg = "https://jolharg.com",
    urlMadHacker = "https://madhackerreviews.com"
}

devBlog, prodBlog,
    devDanDart, prodDanDart,
    devJolHarg, prodJolHarg,
    devM0ORI, prodM0ORI,
    devMadHacker, prodMadHacker ∷ Website
devBlog = Website {
    slug = "blog",
    title = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio) Ham, Musician",
    url = urlBlog developmentUrls,
    urls = developmentUrls,
    siteType = Blog "posts",
    livereload = True,
    build = B.build,
    serve = B.serve
}
prodBlog = devBlog {
    url = urlBlog productionUrls,
    urls = productionUrls,
    livereload = False
}
devDanDart = Website {
    slug = "dandart",
    title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
    url = urlDanDart developmentUrls,
    urls = developmentUrls,
    siteType = Normal,
    livereload = True,
    build = D.build,
    serve = D.serve
}
prodDanDart = devDanDart {
    url = urlDanDart productionUrls,
    urls = productionUrls,
    livereload = False
}
devJolHarg = Website {
    slug = "jolharg",
    title = "JolHarg: Your Software Engineering Partner",
    url = urlJolHarg developmentUrls,
    urls = developmentUrls,
    siteType = Normal,
    livereload = True,
    build = J.build,
    serve = J.serve
}
prodJolHarg = devJolHarg {
    url = urlJolHarg productionUrls,
    urls = productionUrls,
    livereload = False
}
devM0ORI = Website {
    slug = "m0ori",
    title = "M0ORI call sign: Dan Dart, England",
    url = urlHamRadio developmentUrls,
    urls = developmentUrls,
    siteType = Normal,
    livereload = True,
    build = M.build,
    serve = M.serve
}
prodM0ORI = devM0ORI {
    url = urlHamRadio productionUrls,
    urls = productionUrls,
    livereload = False
}
devMadHacker = Website {
    slug = "madhacker",
    title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
    url = urlMadHacker developmentUrls,
    urls = developmentUrls,
    siteType = Blog "reviews",
    livereload = True,
    build = MH.build,
    serve = MH.serve
}
prodMadHacker = devMadHacker {
    url = urlMadHacker productionUrls,
    urls = productionUrls,
    livereload = False
}

development, production ∷ Env
development = [
    devBlog,
    devDanDart,
    devJolHarg,
    devM0ORI,
    devMadHacker
    ]
production = [
    prodBlog,
    prodDanDart,
    prodJolHarg,
    prodM0ORI,
    prodMadHacker
    ]
