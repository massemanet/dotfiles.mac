#!/bin/bash
# -*- mode: shell-script -*-

set -f
[ -z "$1" ] && set +f && exit 1
[ -n "$2" ] && d="$2" || d="."
[ -n "$3" ] && n="-name $3"
find "$d" \
     -follow \
     -path "*/.svn"   -prune -o \
     -path "*/.git"   -prune -o \
     -path "*/.eunit" -prune -o \
     -path "*/.deps"  -prune -o \
     -path "*/#*"     -prune -o \
     -path "*~"       -prune -o \
     -type f $n \
     -exec grep -inIH "$1" {} \;
set +f
