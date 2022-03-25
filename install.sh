#!/usr/bin/env bash

InstallPath=${XDG_DATA_HOME:-$HOME/.local/share}/sisku/static

mkdir -p $InstallPath

npx parcel build src/index.html --dist-dir $InstallPath --no-source-maps