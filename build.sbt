ThisBuild / scalaVersion := "3.2.1"
ThisBuild / organization := "io.github.h-ayat"
ThisBuild / organizationName := "h-ayat"
ThisBuild / organizationHomepage := Some(url("https://h-ayat.github.io/"))

name := "p752"
version := "0.2.0"

lazy val core = project in file("core")
lazy val tiles = (project in file("tiles")).dependsOn(core)
lazy val demo = (project in file("demo")).dependsOn(tiles)

lazy val root = (project in file(".")).aggregate(
  core,
  tiles,
  demo
)

enablePlugins(ScalaNativePlugin)


// MISC


ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/h-ayat/p752"),
    "scm:git@github.com:h-ayat/p752.git"
  )
)

ThisBuild / developers := List(
  Developer(
    id    = "h-ayat",
    name  = "S.H.Ayat",
    email = "fahim.ayat@gmail.com",
    url   = url("https://h-ayat.github.io/")
  )
)

ThisBuild / description := "Minimalist TUI framework for Scala-Native"
ThisBuild / licenses := List("The Unlicense" -> new URL("https://unlicense.org/"))
ThisBuild / homepage := Some(url("https://github.com/h-ayat/p752"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }

ThisBuild / publishTo := {
  val nexus = "https://s01.oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

ThisBuild / publishMavenStyle := true

ThisBuild / versionScheme := Some("early-semver")