#!/bin/bash
cd posts
for i in *comments.md
do
    echo $i
    D=$(echo $i | sed 's/-comments.md//g')
    mkdir $D
    mv $i $D/comments.md
done