#! /bin/sh

cp ./def_index.html index.html
cp css/def.css css/default.css
echo 'complete: cp'

./site build
cp -r _site/* .
./site clean

git add --all
echo 'complete: git add'
