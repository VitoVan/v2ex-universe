#!/bin/bash

if [[ ! -v V2EX_TOKEN ]]; then
    echo "V2EX_TOKEN is not set"
    exit 42
fi

sbcl --load crawler.lisp

git add .
git commit -m "update.sh - $(date)"
git push --set-upstream origin main
