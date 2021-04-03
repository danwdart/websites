FILES=$(find . -name "*.hs" | grep -v .stack-work | grep -v dist-newstyle)
parallel --halt never hlint --refactor --refactor-options=-i ::: $FILES
parallel --halt never stylish-haskell -i ::: $FILES