#!/usr/bin/env bash
for SITE in blog #$(ls sites | grep -v common);
do
    echo Building $SITE...
    echo Clearing $SITE...
    rm -rf .sites/$SITE/*
    echo Copying static content to $SITE...
    cp -rp static/common/* .sites/$SITE
    cp -rp static/$SITE/* .sites/$SITE
    for PAGETYPE in post page
    do
        mkdir tmp_${PAGETYPE}s
        if [[ ! -d sites/$SITE/${PAGETYPE}s ]]
        then
            continue
        fi
        echo Building ${PAGETYPE}s...
        for PAGEFILE in sites/$SITE/${PAGETYPE}s/*.md
        do
            PAGE=$(basename $PAGEFILE .md)
            echo Building $PAGETYPE $PAGE...
            pandoc -f markdown -t html5 \
                --template sites/common/templates/${PAGETYPE}.md \
                $PAGEFILE > \
                tmp_${PAGETYPE}s/$PAGE.md
        done
        echo Combining ${PAGETYPE}s...
        cat tmp_${PAGETYPE}s/*.md > sites/$SITE/${PAGETYPE}/combined.md
        rm -rf tmp_${PAGETYPE}s
    done
    echo Building index.html...
    pandoc -f markdown -t html5 \
        --template sites/common/templates/layout.md \
        -M pages=$(cat sites/$SITE/pages/combined.md)
        -M posts=$(cat sites/$SITE/posts/combined.md)
        sites/$SITE/index.md > \
        .sites/$SITE/index.html
    # rm -rf sites/$SITE/templates/{page,post}s.md
    echo Building 404.html...
    pandoc -f markdown -t html5 \
        --template sites/common/templates/404.md \
        sites/common/404.md > \
        .sites/$SITE/404.html
done