#!/usr/bin/env bash

mkdir -p dist
wget -O dist/nwjs-v0.12.2-osx-x64.zip http://dl.nwjs.io/v0.12.2/nwjs-v0.12.2-osx-x64.zip
unzip -d dist/ dist/nwjs-v0.12.2-osx-x64.zip

./sbt generate
npm install
cp -rf package.json ./app ./node_modules ./target ./dist/nwjs-v0.12.2-osx-x64/nwjs.app/Contents/Resources/app.nw/
cp -rf ./dist/nwjs-v0.12.2-osx-x64/nwjs.app ./dist/fast-links.app
