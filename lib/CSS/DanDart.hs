{-

a.social {
    font-size: 24px;
    margin: 0 4px;
}

a.social, a.social-pump {
    filter: contrast(0);
    transition: filter 0.5s;
}

a.social:hover, a.social-pump:hover {
    filter: contrast(1);
}

a.social-pump img {
    width: 24px;
    margin: -10px 4px 0 4px;
}

.social-row {
    overflow: auto;
    display: block;
    text-align: right;
}

.social-row {
    overflow: auto hidden;
    -ms-overflow-style: none;
    scrollbar-width: none;
}

.social-row::-webkit-scrollbar {
    display: none;
}

.social-inside {
    display: flex;
}

@media(max-width: 768px) {
    .page {
        margin-top: 40px;
    }
}

@media(max-width: 576px) {
    .container-fluid {
        margin-top: 80px;
    }
}

-}

module CSS.DanDart where

