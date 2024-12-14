{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

module Data.Env where

import Build.Blog        qualified as Blog
import Build.BlogJolHarg qualified as BlogJolHarg
import Build.BlogM0ORI   qualified as BlogM0ORI
import Build.DanDart     qualified as DanDart
import Build.JolHarg     qualified as JolHarg
import Build.M0ORI       qualified as M0ORI
import Build.MadHacker   qualified as MadHacker
import Control.Lens
import Data.Env.Types

productionUrls ∷ Urls
productionUrls = Urls {
    _urlDanDart = "https://dandart.co.uk",
    _urlHamRadio = "https://m0ori.com",
    _urlBlogHamRadio = "https://blog.m0ori.com",
    _urlBlog = "https://blog.dandart.co.uk",
    _urlBlogJolHarg = "https://blog.jolharg.com",
    _urlJolHarg = "https://jolharg.com",
    _urlMadHacker = "https://madhackerreviews.com"
}

prodBlog,
    prodDanDart,
    prodJolHarg,
    prodBlogJolHarg,
    prodM0ORI,
    prodBlogM0ORI,
    prodMadHacker ∷ Website
prodBlog = Website {
    _slug = "blog",
    _title = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio) Ham, Musician",
    {- _keywords = [
        "dan",
        "dart",
        "dandart",
        "daniel dart",
        "dan dart",
        "exmouth",
        "exeter",
        "devon",
        "england",
        "united kingdom",
        "uk",
        "en_GB",
        "gb",
        "Great Britain",
        "Britain",
        "blog",
        "software",
        "engineer",
        "mathematics",
        "lover",
        "radio",
        "ham",
        "haskell",
        "typescript",
        "react.js",
        "react",
        "php",
        "javascript",
        "css",
        "coffee",
        "coffeescript",
        "laravel",
        "zend",
        "framework",
        "linux",
        "gnu",
        "express.js",
        "ubuntu",
        "debian"
        ],-}
    _description = "The blog of Dan Dart. Includes life-changing observations and scientific breakthroughs, as well as interesting content from around the world.",
    _imgUrl = "https://dandart.co.uk/img/header.png",
    _baseUrl = productionUrls ^. urlBlog,
    _pageUrl = productionUrls ^. urlBlog,
    _urls = productionUrls,
    _siteType = Blog {
        _atomTitle = "Dan Dart's Blog: Software Engineer, Mathematics Lover, Radio) Ham, Musician",
        _atomUrl = "https://blog.dandart.co.uk/atom.xml"
    },
    _email = "blog@dandart.co.uk",
    _livereload = False,
    _build = Blog.build
}
prodDanDart = Website {
    _slug = "dandart",
    _title = "Dan Dart: Software Engineer, Mathematics Lover, Radio Ham, Musician",
    {- _keywords = [
        "dan",
        "dart",
        "dandart",
        "daniel dart",
        "dan dart",
        "exmouth",
        "exeter",
        "devon",
        "england",
        "united kingdom",
        "uk",
        "en_GB",
        "gb",
        "Great Britain",
        "Britain",
        "software",
        "engineer",
        "mathematics",
        "lover",
        "radio",
        "ham",
        "haskell",
        "typescript",
        "react.js",
        "react",
        "php",
        "javascript",
        "css",
        "coffee",
        "coffeescript",
        "laravel",
        "zend",
        "framework",
        "linux",
        "gnu",
        "express.js",
        "ubuntu",
        "debian"
        ],
    -}
    _description = "Dan Dart works on a large collection of software and is interested in mathematics, physics, chemistry, radio and linguistics.",
    _imgUrl = "https://dandart.co.uk/img/header.png",
    _baseUrl = productionUrls ^. urlDanDart,
    _pageUrl = productionUrls ^. urlDanDart,
    _urls = productionUrls,
    _siteType = Normal,
    _email = "website@dandart.co.uk",
    _livereload = False,
    _build = DanDart.build
}
prodJolHarg = Website {
    _slug = "jolharg",
    _title = "JolHarg: Your Software Engineering Partner",
    {- _keywords = [
        "jolharg",
        "dan",
        "dart",
        "dandart",
        "daniel dart",
        "dan dart",
        "exmouth",
        "exeter",
        "devon",
        "england",
        "united kingdom",
        "uk",
        "en_GB",
        "gb",
        "Great Britain",
        "Britain",
        "software",
        "dan",
        "dart",
        "software",
        "engineer",
        "mathematics",
        "haskell",
        "php",
        "javascript",
        "react",
        "react.js",
        "hoogle",
        "help",
        "computing",
        "computer",
        "serverless",
        "npm",
        "hask",
        "ask",
        "question",
        "haskell",
        "typescript",
        "react.js",
        "react",
        "css",
        "coffee",
        "coffeescript",
        "laravel",
        "zend",
        "framework",
        "linux",
        "gnu",
        "express.js",
        "ubuntu",
        "debian"
        ], -}
    _description = "Dan Dart can provide you with all kinds of software engineering including fully-functioning web and phone applications.",
    _imgUrl = "https://jolharg.com/img/header.png",
    _baseUrl = productionUrls ^. urlJolHarg,
    _pageUrl = productionUrls ^. urlJolHarg,
    _urls = productionUrls,
    _siteType = Normal,
    _email = "website@jolharg.com",
    _livereload = False,
    _build = JolHarg.build
}
prodBlogJolHarg = Website {
    _slug = "blogjolharg",
    _title = "JolHarg: Software Blog",
    {- _keywords = [
        "jolharg",
        "blog",
        "dandart",
        "daniel dart",
        "dan dart",
        "exmouth",
        "exeter",
        "devon",
        "england",
        "united kingdom",
        "uk",
        "en_GB",
        "gb",
        "Great Britain",
        "Britain",
        "software",
        "engineer",
        "mathematics",
        "lover",
        "radio",
        "ham",
        "haskell",
        "typescript",
        "react.js",
        "react",
        "php",
        "javascript",
        "css",
        "coffee",
        "coffeescript",
        "laravel",
        "zend",
        "framework",
        "linux",
        "gnu",
        "express.js",
        "ubuntu",
        "debian"
        ], -}
    _description = "JolHarg's blog covers various pieces of technology, code and tutorials to help make your life easier.",
    _imgUrl = "https://jolharg.com/img/header.png",
    _baseUrl = productionUrls ^. urlBlogJolHarg,
    _pageUrl = productionUrls ^. urlBlogJolHarg,
    _urls = productionUrls,
    _siteType = Blog {
        _atomTitle = "JolHarg: Software Blog",
        _atomUrl = "https://blog.jolharg.com/atom.xml"
    },
    _email = "blog@jolharg.com",
    _livereload = False,
    _build = BlogJolHarg.build
}
prodM0ORI = Website {
    _slug = "m0ori",
    _title = "M0ORI call sign: Dan Dart, England",
    {- _keywords = [
        "dan",
        "dart",
        "dandart",
        "daniel dart",
        "dan dart",
        "exmouth",
        "exeter",
        "devon",
        "england",
        "united kingdom",
        "uk",
        "en_GB",
        "gb",
        "Great Britain",
        "Britain",
        "haskell",
        "typescript",
        "react.js",
        "react",
        "radio",
        "call",
        "sign",
        "ham",
        "m0ori",
        "yaesu",
        "qrz"
        ]
    -}
    _description = "The M0ORI callsign is owned by Dan Dart located in England. He works on HF and VHF in Exmouth.",
    _imgUrl = "https://dandart.co.uk/img/header.png",
    _baseUrl = productionUrls ^. urlHamRadio,
    _pageUrl = productionUrls ^. urlHamRadio,
    _urls = productionUrls,
    _siteType = Normal,
    _email = "website@m0ori.com",
    _livereload = False,
    _build = M0ORI.build
}
prodBlogM0ORI = Website {
    _slug = "blogm0ori",
    _title = "The Blog of M0ORI: Interesting Radio Observations",
    _description = "My radio blog covers interesting observations I have had whilst working on ham bands.",
    _imgUrl = "https://dandart.co.uk/img/header.png",
    _baseUrl = productionUrls ^. urlBlogHamRadio,
    _pageUrl = productionUrls ^. urlBlogHamRadio,
    _urls = productionUrls,
    _siteType = Blog {
        _atomTitle = "The Blog of M0ORI: Interesting Radio Observations",
        _atomUrl = "https://blog.m0ori.com/atom.xml"
    },
    _email = "blog@m0ori.com",
    _livereload = False,
    _build = BlogM0ORI.build
}
prodMadHacker = Website {
    _slug = "madhacker",
    _title = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
    {- _keywords = [
        "exmouth",
        "exeter",
        "devon",
        "england",
        "united kingdom",
        "uk",
        "en_GB",
        "gb",
        "Great Britain",
        "Britain",
        "mad",
        "hacker",
        "tech",
        "technology",
        "reviews",
        "review",
        "dan",
        "dart",
        "dandart",
        "daniel dart",
        "dan dart",
        "haskell",
        "typescript",
        "react.js",
        "react"
        ]
    -}
    _description = "Find tech and software reviews with a hackability twist, right here! Requests are accepted and review models are always non-sponsored.",
    _imgUrl = "https://dandart.co.uk/img/header.png",
    _baseUrl = productionUrls ^. urlMadHacker,
    _pageUrl = productionUrls ^. urlMadHacker,
    _urls = productionUrls,
    _siteType = Blog {
        _atomTitle = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
        _atomUrl = "https://madhackerreviews.com/atom.xml"
    },
    _email = "madhacker@dandart.co.uk", -- TODO add MX
    _livereload = False,
    _build = MadHacker.build
}

production ∷ Env
production = [
    prodBlog,
    prodDanDart,
    prodJolHarg,
    prodBlogJolHarg,
    prodBlogM0ORI,
    prodM0ORI,
    prodMadHacker
    ]
