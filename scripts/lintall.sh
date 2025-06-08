FILES=$(find . -name "*.hs" | grep -v .stack-work | grep -v "dist-*")
echo "will cite" | parallel --citation 2> /dev/null
parallel --halt never hlint --refactor --refactor-options=-i ::: $FILES
parallel --halt never stylish-haskell -i ::: $FILES