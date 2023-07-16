#!/usr/bin/env bash

git checkout main
git branch gh-pages
git checkout gh-pages
git rm -r .
git checkout main -- ./public/*
git commit -am "Update github pages"
git push origin gh-pages
git checkout main
