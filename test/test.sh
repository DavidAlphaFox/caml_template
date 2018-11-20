#!/bin/sh

NEW_FILE=coleridge.html
SAVED_FILE=coleridge.html.saved

cd examples/filetest

if ./filetest; then
    if [ -e $SAVED_FILE ]; then
        if cmp -s $NEW_FILE $SAVED_FILE; then
            echo "Test passed."
        else
            echo "Test failed; $NEW_FILE and $SAVED_FILE are different."
        fi
    else
        cp $NEW_FILE $SAVED_FILE
        echo "Created $SAVED_FILE."
    fi
else
    echo "Test failed."
fi

cd ../..
