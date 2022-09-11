{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Data.Env where

import qualified Build.Blog        as Blog
import qualified Build.BlogJolHarg as BlogJolHarg
import qualified Build.DanDart     as DanDart
import qualified Build.JolHarg     as JolHarg
import qualified Build.M0ORI       as M0ORI
import qualified Build.MadHacker   as MadHacker
import           Data.Env.Types

productionUrls ∷ Urls
productionUrls = Urls {
    urlDanDart = "https://dandart.co.uk",
    urlHamRadio = "https://m0ori.com",
    urlBlog = "https://blog.dandart.co.uk",
    urlBlogJolHarg = "https://blog.jolharg.com",
    urlJolHarg = "https://jolharg.com",
    urlMadHacker = "https://madhackerreviews.com"
}

prodBlog,
    prodDanDart,
    prodJolHarg,
    prodBlogJolHarg,
    prodM0ORI,
    prodMadHacker ∷ Website
prodBlog = Website {
    slug = "blog",
    title = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio) Ham, Musician",
    url = urlBlog productionUrls,
    urls = productionUrls,
    siteType = Blog,
    livereload = False,
    build = Blog.build
}
prodDanDart = Website {
    slug = "dandart",
    title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
    url = urlDanDart productionUrls,
    urls = productionUrls,
    siteType = Normal,
    livereload = False,
    build = DanDart.build
}
prodJolHarg = Website {
    slug = "jolharg",
    title = "JolHarg: Your Software Engineering Partner",
    url = urlJolHarg productionUrls,
    urls = productionUrls,
    siteType = Normal,
    livereload = False,
    build = JolHarg.build
}
prodBlogJolHarg = Website {
    slug = "blogjolharg",
    title = "JolHarg: Software Blog",
    url = urlBlogJolHarg productionUrls,
    urls = productionUrls,
    siteType = Blog,
    livereload = False,
    build = BlogJolHarg.build
}
prodM0ORI = Website {
    slug = "m0ori",
    title = "M0ORI call sign: Dan Dart, England",
    url = urlHamRadio productionUrls,
    urls = productionUrls,
    siteType = Normal,
    livereload = False,
    build = M0ORI.build
}
prodMadHacker = Website {
    slug = "madhacker",
    title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
    url = urlMadHacker productionUrls,
    urls = productionUrls,
    siteType = Blog,
    livereload = False,
    build = MadHacker.build
}

production ∷ Env
production = [
    prodBlog,
    prodDanDart,
    prodJolHarg,
    prodBlogJolHarg,
    prodM0ORI,
    prodMadHacker
    ]
