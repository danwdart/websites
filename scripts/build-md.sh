#!/usr/bin/env nix-shell
#! nix-shell -I nixpkgs=channel:nixos-unstable -p pandoc -i bash
for SITE in blog #$(ls sites | grep -v common);
do
    echo Building $SITE...
    echo Clearing $SITE...
    rm -rf .sites/$SITE/*
    echo Copying static content to $SITE...
    cp -rp static/common/* .sites/$SITE
    cp -rp static/$SITE/* .sites/$SITE

    mkdir tmp_posts
    if [[ ! -d sites/$SITE/posts ]]
    then
        continue
    fi
    echo Building posts...
    for POSTFILE in $(find sites/$SITE/posts/*/*/*.md)
    do
        POST=$(basename $POSTFILE .md)
        echo Building $POSTTYPE $POST...
        pandoc -f markdown -t html5 \
            --template sites/common/templates/post.md \
            $POSTFILE > \
            tmp_posts/$POST.html
    done
    echo Combining posts...
    cat $(ls tmp_posts/2*.html | tac) > sites/$SITE/posts/combined.html
    rm -rf tmp_posts

    mkdir tmp_pages
    if [[ ! -d sites/$SITE/pages ]]
    then
        continue
    fi
    echo Building pages...
    for PAGEFILE in $(find sites/$SITE/pages/*.md)
    do
        PAGE=$(basename $PAGEFILE .md)
        echo Building $PAGETYPE $PAGE...
        pandoc -f markdown -t html5 \
            --template sites/common/templates/page.md \
            -M posts="$(cat sites/$SITE/posts/combined.html)" \
            $PAGEFILE > \
            tmp_pages/$PAGE.html
    done
    echo Combining pages...
    cat tmp_pages/*.html > sites/$SITE/pages/combined.html
    rm -rf tmp_pages
    
    echo Building index.html...
    pandoc -f markdown -t html5 \
        --template sites/common/templates/layout.md \
        -M pages="$(cat sites/$SITE/pages/combined.html)" \
        -M posts="$(cat sites/$SITE/posts/combined.html)" \
        sites/$SITE/index.md > \
        .sites/$SITE/index.html

    # rm -rf sites/$SITE/{page,post}s/combined.html
    echo Building 404.html...
    pandoc -f markdown -t html5 \
        --template sites/common/templates/404.md \
        sites/common/404.md > \
        .sites/$SITE/404.html
done