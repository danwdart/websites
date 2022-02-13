{-# LANGUAGE OverloadedStrings #-}

module CSS.Common where

import Clay

common :: Css
common = do
    star ? fontFamily ["Caudex"] [sansSerif]
    html ? ""
    body ? backgroundColor (rgb 0xe 0xe 0xe)
    (p <> ul) ? margin nil nil nil nil
    (a <> a # (hover <> active <> visited)) ? color "purple"
    ".bg-primary" ? backgroundColor "#ddd" -- !important
    ".navbar-dark .navbar-nav .nav-link" # (id <> hover <> active <> focus) ? do
        color purple
        boxShadow none
    ".navbar-nav" ? overflow auto
    ".nav-item" ? flex nil nil auto -- flex: 0 0 auto;
    ".nav-link" ? transition transform (sec 0.5) easeInOut -- transition: transform 0.5s ease-in-out !important;
    ".nav-link" # hover ? transform scale 1.1 -- transform: scale(1.1);
    ".row" ? margin nil nil nil nil
    ".page" ? do
        display none

{-

    


.nav-link {
    

.row {
    margin: 0;
}

.page {
    display: none;
    position: absolute;
    width: 100%;
    left: 0;
}

input:checked ~ label > a {
    transform: scale(1.1);
    font-weight: bold;
}

input:checked ~ .page {
    display: block;
}

.card {
    margin-bottom: 30px;
    padding: 0;
    background-color: transparent;
    border: none;
    display: inline-block;
}

.card > a {
    color: black;
    text-decoration: none;
}

.card-body {
    margin: 0 15px;
    background-color: white;
}

.card img {
    height: 200px;
    width: 80%;
    object-fit: cover;
    /* border-radius: 10px; */
    margin-bottom: 20px;
}

.card .card-img-top-github {
    object-fit: contain;
}

.card .card-body {
    box-shadow: 3px 3px 5px #ccc;
}

.card .card-text {
    max-width: 100%;
}

.card .card-text .description {
    display: inline-block;
    height: 50px;
}

.card strong {
    line-height: 40px;
}

.card .btn {
    border-radius: 0;
    line-height: 20px;
}

.bg-light {
    background: white;
}

.form-group:last-of-type {
    margin-bottom: 0;
}

.title {
    color: purple;
}

.navbar-nav, .navbar > div {
    overflow: auto hidden;
    -ms-overflow-style: none;
    scrollbar-width: none;
}

.navbar-nav::-webkit-scrollbar,
.navbar > div::-webkit-scrollbar {
    display: none;
}

@media(max-width: 768px) {
    .page {
        margin-top: 30px;
    }
}

@media(max-width: 576px) {
    .container-fluid {
        margin-top: 80px;
    }
}

-}