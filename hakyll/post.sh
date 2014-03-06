#!/bin/sh

ymd=`date '+%F'`
title="$ymd-$1"

cp posts/default.markdown posts/$title.md
vim posts/$title.md
