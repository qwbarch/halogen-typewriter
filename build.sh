#!/usr/bin/env bash

nix develop --command spago bundle-app --no-install --to public/index.js
