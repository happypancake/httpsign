#!/bin/bash
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

cd $DIR
rm -rf ./doc
git clone . ./doc
cd doc
git checkout master -B gh-pages
git rm -rf .
cd ..
./rebar doc
cd doc
git add .
git commit -m "generated docs"
git push origin gh-pages:gh-pages -f
cd ..
git push origin gh-pages:gh-pages -f
rm -rf ./doc/.git


