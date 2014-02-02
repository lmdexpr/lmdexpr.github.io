#! /bin/sh

cp ./def_index.html index.html
./site build
cp -r _site/* .
./site clean

git add --all
