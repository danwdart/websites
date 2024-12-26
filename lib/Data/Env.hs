{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE StrictData        #-}

module Data.Env where

import Build.Blog                qualified as Blog
import Build.BlogJolHarg         qualified as BlogJolHarg
import Build.BlogM0ORI           qualified as BlogM0ORI
import Build.DanDart             qualified as DanDart
import Build.JolHarg             qualified as JolHarg
import Build.M0ORI               qualified as M0ORI
import Build.MadHacker           qualified as MadHacker
import Control.Lens
import Data.Env.Types
import Network.URI.Static
import Text.Email.QuasiQuotation qualified as QE

productionUrls ∷ Urls
productionUrls = Urls {
    _urlDanDart = [uri|https://dandart.co.uk|],
    _urlHamRadio = [uri|https://m0ori.com|],
    _urlBlogHamRadio = [uri|https://blog.m0ori.com|],
    _urlBlog = [uri|https://blog.dandart.co.uk|],
    _urlBlogJolHarg = [uri|https://blog.jolharg.com|],
    _urlJolHarg = [uri|https://jolharg.com|],
    _urlMadHacker = [uri|https://madhackerreviews.com|]
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
    _title = "Dan Dart's Blog: Software, Mathematics, Radio, Music",
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
    _previewImgUrl = [uri|https://blog.dandart.co.uk/img/preview.png|],
    _baseUrl = productionUrls ^. urlBlog,
    _pageUrl = productionUrls ^. urlBlog,
    _sitemapUrl = [uri|https://blog.dandart.co.uk/sitemap.xml|],
    _urls = productionUrls,
    _breadcrumb = Breadcrumb [("Blog", Nothing)],
    _siteType = Blog {
        _atomTitle = "Dan Dart's Blog: Software, Maths, Radio, Music",
        _atomUrl = [uri|https://blog.dandart.co.uk/atom.xml|]
    },
    _email = [QE.email|blog@dandart.co.uk|],
    _openGraphInfo = OGWebsite,
    _livereload = False,
    _build = Blog.build
}
prodDanDart = Website {
    _slug = "dandart",
    _title = "Dan Dart: Software, Maths, Radio, Music",
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
    _previewImgUrl = [uri|https://dandart.co.uk/img/preview.png|],
    _baseUrl = productionUrls ^. urlDanDart,
    _pageUrl = productionUrls ^. urlDanDart,
    _sitemapUrl = [uri|https://dandart.co.uk/sitemap.xml|],
    _urls = productionUrls,
    _breadcrumb = Breadcrumb [("Dan Dart", Nothing)],
    _siteType = Normal,
    _email = [QE.email|website@dandart.co.uk|],
    _openGraphInfo = OGProfile $ OpenGraphProfile {
        _ogProfileFirstName = "Dan",
        _ogProfileLastName = "Dart",
        _ogProfileUsername = "dandart",
        _ogProfileGender = "non-binary"
    },
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
    _previewImgUrl = [uri|https://jolharg.com/img/preview.png|],
    _baseUrl = productionUrls ^. urlJolHarg,
    _pageUrl = productionUrls ^. urlJolHarg,
    _sitemapUrl = [uri|https://jolharg.com/sitemap.xml|],
    _urls = productionUrls,
    _breadcrumb = Breadcrumb [("JolHarg", Nothing)],
    _siteType = Normal,
    _email = [QE.email|website@jolharg.com|],
    _openGraphInfo = OGWebsite,
    _livereload = False,
    _build = JolHarg.build
}
prodBlogJolHarg = Website {
    _slug = "blogjolharg",
    _title = "JolHarg: Software and Technology Blog",
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
    _previewImgUrl = [uri|https://blog.jolharg.com/img/preview.png|],
    _baseUrl = productionUrls ^. urlBlogJolHarg,
    _pageUrl = productionUrls ^. urlBlogJolHarg,
    _sitemapUrl = [uri|https://blog.jolharg.com/sitemap.xml|],
    _urls = productionUrls,
    _breadcrumb = Breadcrumb [("JolHarg Blog", Nothing)],
    _siteType = Blog {
        _atomTitle = "JolHarg: Software and Technology Blog",
        _atomUrl = [uri|https://blog.jolharg.com/atom.xml|]
    },
    _email = [QE.email|blog@jolharg.com|],
    _openGraphInfo = OGWebsite,
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
    _previewImgUrl = [uri|https://m0ori.com/img/preview.png|],
    _baseUrl = productionUrls ^. urlHamRadio,
    _pageUrl = productionUrls ^. urlHamRadio,
    _sitemapUrl = [uri|https://m0ori.com/sitemap.xml|],
    _urls = productionUrls,
    _breadcrumb = Breadcrumb [("M0ORI", Nothing)],
    _siteType = Normal,
    _email = [QE.email|website@m0ori.com|],
    _openGraphInfo = OGWebsite,
    _livereload = False,
    _build = M0ORI.build
}
prodBlogM0ORI = Website {
    _slug = "blogm0ori",
    _title = "The Blog of M0ORI: Interesting Radio Observations",
    _description = "My radio blog covers interesting observations I have had whilst working on ham bands.",
    _previewImgUrl = [uri|https://blog.m0ori.com/img/preview.png|],
    _baseUrl = productionUrls ^. urlBlogHamRadio,
    _pageUrl = productionUrls ^. urlBlogHamRadio,
    _sitemapUrl = [uri|https://blog.m0ori.com/sitemap.xml|],
    _urls = productionUrls,
    _breadcrumb = Breadcrumb [("M0ORI Blog", Nothing)],
    _siteType = Blog {
        _atomTitle = "The Blog of M0ORI: Interesting Radio Observations",
        _atomUrl = [uri|https://blog.m0ori.com/atom.xml|]
    },
    _email = [QE.email|blog@m0ori.com|],
    _openGraphInfo = OGWebsite,
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
    _previewImgUrl = [uri|https://madhackerreviews.com/img/preview.png|],
    _baseUrl = productionUrls ^. urlMadHacker,
    _pageUrl = productionUrls ^. urlMadHacker,
    _sitemapUrl = [uri|https://madhackerreviews.com/sitemap.xml|],
    _urls = productionUrls,
    _breadcrumb = Breadcrumb [("Mad Hacker Reviews", Nothing)],
    _siteType = Blog {
        _atomTitle = "The Mad Hacker: Tech Reviews by a crazy computer enthusiast",
        _atomUrl = [uri|https://madhackerreviews.com/atom.xml|]
    },
    _email = [QE.email|madhacker@dandart.co.uk|], -- TODO add MX
    _openGraphInfo = OGWebsite,
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
