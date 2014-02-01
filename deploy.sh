#! /bin/sh
#deployツールもHaskellで書こうと思ってた時期が僕にもありました

cp ./def_index.html ./index.html
./site build
cp -r _site/* .
./site clean
