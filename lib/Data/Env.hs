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
import Control.Monad.IO.Class
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
            build = do
                liftIO . putStrLn $ "B.build dev"
                B.build,
            serve = do
                liftIO . putStrLn $ "B.serve dev"
                B.serve
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
            build = do
                liftIO . putStrLn $ "D.build dev"
                D.build,
            serve = do
                liftIO . putStrLn $ "D.serve dev"
                D.serve
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
            build = do
                liftIO . putStrLn $ "J.build dev"
                J.build,
            serve = do
                liftIO . putStrLn $ "J.serve dev"
                J.serve
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
            build = do
                liftIO . putStrLn $ "M.build dev"
                M.build,
            serve = do
                liftIO . putStrLn $ "M.serve dev"
                M.serve
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
            build = do
                liftIO . putStrLn $ "MH.build dev"
                MH.build,
            serve = do
                liftIO . putStrLn $ "MH.serve dev"
                MH.serve
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
            build = do
                liftIO . putStrLn $ "B.build prod"
                B.build,
            serve = do
                liftIO . putStrLn $ "B.serve prod"
                B.serve
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
            build = do
                liftIO . putStrLn $ "D.build prod"
                D.build,
            serve = do
                liftIO . putStrLn $ "D.serve prod"
                D.serve
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
            build = do
                liftIO . putStrLn $ "J.build prod"
                J.build,
            serve = do
                liftIO . putStrLn $ "J.serve prod"
                J.serve
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
            build = do
                liftIO . putStrLn $ "M.build prod"
                M.build,
            serve = do
                liftIO . putStrLn $ "M.serve prod"
                M.serve
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
            build = do
                liftIO . putStrLn $ "MH.build prod"
                MH.build,
            serve = do
                liftIO . putStrLn $ "MH.serve prod"
                MH.serve
        }
    )
    ]
