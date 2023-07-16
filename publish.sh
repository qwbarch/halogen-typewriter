#!/usr/bin/env bash

if ! git diff-index --quiet HEAD --; then
  echo "There are uncommitted changes. Please commit or stash them before running the script."
  exit 1
fi

current_branch=$(git rev-parse --abbrev-ref HEAD)

if [ "$current_branch" = "main" ]; then
  if git rev-parse --quiet --verify "gh-pages" >/dev/null; then
    git checkout gh-pages
    find . -mindepth 1 ! -regex '^./.git\(/.*\)?' -delete
    git checkout main -- .
    ./build.sh
  else
    ./build.sh
    git checkout --orphan gh-pages
  fi
  
  rm .gitignore
  git reset
  git add public
  git clean -fd

  mv public/* .
  rm -r public

  git add --all -- :!.spago
  git commit -am "Update github pages."

  git push origin gh-pages --force
  git checkout main

  echo "Succesfully pushed to github-pages!"
else
  echo "Please switch to the main branch before publishing to github pages"
fi
