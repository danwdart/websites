{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.Env where

import qualified Build.Blog as B
import qualified Build.DanDart as D
import qualified Build.JolHarg as J
import qualified Build.M0ORI as M
import qualified Build.MadHacker as MH
import           Data.Env.Types

developmentUrls, productionUrls :: Urls
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

development, production ∷ Env
development = [
    (
        "blog",
        Website {
            slug = "blog",
            title = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio) Ham, Musician",
            url = "http://blog.localhost:8080",
            urls = developmentUrls,
            siteType = Blog "posts",
            livereload = False,
            endpoint = "http://localhost:3000/dev",
            build = B.build,
            serve = B.serve
        }
    ),
    (
        "dandart",
        Website {
            slug = "dandart",
            title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "http://dandart.localhost:8080",
            urls = developmentUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "http://localhost:3000/dev",
            build = D.build,
            serve = D.serve
        }
    ),
    (
        "jolharg",
        Website {
            slug = "jolharg",
            title = "JolHarg: Your Software Engineering Partner",
            url = "http://jolharg.localhost:8080",
            urls = developmentUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "http://localhost:3000/dev",
            build = J.build,
            serve = J.serve
        }
    ),
    (
        "m0ori",
        Website {
            slug = "m0ori",
            title = "M0ORI call sign: Dan Dart, England",
            url = "http://m0ori.localhost:8080",
            urls = developmentUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "http://localhost:3000/dev",
            build = M.build,
            serve = M.serve
        }
    ),
    (
        "madhacker",
        Website {
            slug = "madhacker",
            title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
            url = "http://madhacker.localhost:8080",
            urls = developmentUrls,
            siteType = Blog "reviews",
            livereload = False,
            endpoint = "http://localhost:3000/dev",
            build = MH.build,
            serve = MH.serve
        }
    )
    ]

production = [
    (
        "blog",
        Website {
            slug = "blog",
            title = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "https://blog.dandart.co.uk",
            urls = productionUrls,
            siteType = Blog "posts",
            livereload = False,
            endpoint = "https://api.jolharg.com",
            build = B.build,
            serve = B.serve
        }
    ),
    (
        "dandart",
        Website {
            slug = "dandart",
            title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
            url = "https://dandart.co.uk",
            urls = productionUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "https://api.jolharg,com",
            build = D.build,
            serve = D.serve
        }
    ),
    (
        "jolharg",
        Website {
            slug = "jolharg",
            title = "JolHarg: Your Software Engineering Partner",
            url = "https://jolharg.com",
            urls = productionUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "https://api.jolharg,com",
            build = J.build,
            serve = J.serve
        }
    ),
    (
        "m0ori",
        Website {
            slug = "m0ori",
            title = "M0ORI call sign: Dan Dart, England",
            url = "https://m0ori.com",
            urls = productionUrls,
            siteType = Normal,
            livereload = False,
            endpoint = "https://api.jolharg,com",
            build = M.build,
            serve = M.serve
        }
    ),
    (
        "madhacker",
        Website {
            slug = "madhacker",
            title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
            url = "https://madhackerreviews.com",
            urls = productionUrls,
            siteType = Blog "reviews",
            livereload = False,
            endpoint = "https://api.jolharg,com",
            build = MH.build,
            serve = MH.serve
        }
    )
    ]
