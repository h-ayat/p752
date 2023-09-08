# p752
Basic TUI engine for scala-native.

## Overview
This project consists of three modules:
* core : the engine and core system
* demo: primitive samples 
* tiles: Some pre-defined components (called tiles) including:
  * Autocomplete
  * HorizontalList
  * Input
  * Prompt
  * Table
  * VerticalList

## Setup
Add the following to `build.sbt` in your scala-native project: (you can use `p752-core` instead of `p752-tiles` to exclude predefined components)
```scala
libraryDependencies += "io.github.h-ayat" %%% "p752-tiles" % "0.4.0"
```

## Demo
![out](https://user-images.githubusercontent.com/4332421/227467357-2ec6417f-a7af-466a-80e2-7a77775f2b1f.gif)
