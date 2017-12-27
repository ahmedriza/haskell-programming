#!/bin/bash

echo 'Unpacking dependencies...'
rm -rf libs/*
cd libs
python2 ../unpack.py
cd ..
echo 'Running hasktags...'
hasktags --ignore-close-implementation --etags .
#
# Revisit the TAGS in emacs:
# M-x visit-tags-table
# 
