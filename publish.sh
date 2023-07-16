#!/usr/bin/env bash

if ! git diff-index --quiet HEAD --; then
  echo "There are uncommitted changes. Please commit or stash them before running the script."
  exit 1
fi

if [ -z "$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null)" ]; then
  echo "The current branch is not tracking a remote branch. Please push your changes first."
  exit 1
fi

current_branch=$(git rev-parse --abbrev-ref HEAD)

if [ "$current_branch" = "main" ]; then
  if git rev-parse --quiet --verify "gh-pages" >/dev/null; then
    git checkout gh-pages
  else
    git checkout --orphan gh-pages
  fi

  mv public/index.js index.js
  rm -r public
  find . -mindepth 1 ! -regex "^./.git(/.*)?" -name "index.js" -delete
  git checkout main -- ./public/*
  git add .
  git commit -m "Update github pages"
  git push origin gh-pages
  git checkout main

  echo "Succesfully pushed to github-pages!"
else
  echo "Please switch to the main branch before publishing to github pages"
fi
