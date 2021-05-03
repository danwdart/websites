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
        pandoc -f markdown+raw_attribute -t html5 \
            --template sites/common/templates/post.md \
            $POSTFILE > \
            tmp_posts/$POST.html
    done
    echo Combining posts...
    echo -e "\`\`\`{=html}" > sites/common/templates/posts.md
    cat $(ls tmp_posts/2*.html | tac) >> sites/common/templates/posts.md
    echo -e "\n\`\`\`" >> sites/common/templates/posts.md
    rm -rf tmp_posts

    mkdir tmp_pages
    echo Building pages...
    for PAGEFILE in $(find sites/$SITE/pages/*.md)
    do
        PAGE=$(basename $PAGEFILE .md)
        echo Building $PAGETYPE $PAGE...
        pandoc -f markdown+raw_attribute-tex_math_dollars -t html5 \
            --data-dir sites/common/templates \
            --template sites/common/templates/page.md \
            $PAGEFILE > \
            tmp_pages/$PAGE.html
    done
    echo Combining pages...
    echo -e "\`\`\`{=html}\n" > sites/common/templates/pages.md
    cat tmp_pages/*.html >> sites/common/templates/pages.md
    echo -e "\n\`\`\`" >> sites/common/templates/pages.md
    rm -rf tmp_pages
    
    echo Building index.html...
    pandoc -f markdown+raw_attribute-tex_math_dollars -t html5 \
        --data-dir sites/common/templates \
        --template sites/common/templates/layout.md \
        sites/$SITE/index.md > \
        .sites/$SITE/index.html

    echo Building 404.html...
    pandoc -f markdown+raw_attribute-tex_math_dollars -t html5 \
        --data-dir sites/common/templates \
        --template sites/common/templates/404.md \
        sites/common/404.md > \
        .sites/$SITE/404.html
    
    # rm -rf sites/common/templates/{page,post}s.md
done