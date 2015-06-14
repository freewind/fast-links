Fast links
==========

A desktop application which can manage and query links very easily.

Build
=====

Only tested for Mac:

1. `./build.sh` to download necessary libraries and build `./dist/fast-links.app`
2. Copy `./dist/fast-links.app` to you `applications` dir
3. Open `Applications/fast-links.app`, it will ask you to choose a data file
4. Choose an existing data file to start using it (can't create one for now)

Skills
======

1. Recommend tools like `Apptivate` to give a global keyshort to start it, make it faster

Technique Stack
================

[Scala.js](http://www.scala-js.org/) + [widok](https://github.com/widok/widok) + [nodewebkit](https://github.com/nwjs/nw.js/)
