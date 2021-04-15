#!/bin/sh
SRCFILES=$(find src -name "*.hs")
for SRCFILE in $SRCFILES
do
    TESTFILE=$(echo $SRCFILE | sed 's/src/test/' | sed 's/\.hs/Spec\.hs/')
    TESTDIR=$(dirname $TESTFILE)
    mkdir -p $TESTDIR
    if [[ -f $TESTFILE ]]
    then
        echo Test file $TESTFILE exists, not recreating.
    else
        touch $TESTFILE
        MODULE=$(echo $SRCFILE | sed 's/src\///' | sed 's/\.hs//' | sed 's/\//\./g')
        echo -e "module ${MODULE}Spec where\n\nimport Test.Hspec\n\nspec :: Spec\nspec = pure ()" > $TESTFILE
    fi
done