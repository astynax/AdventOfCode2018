version := "0.1.0-SNAPSHOT"

scalaVersion := "2.13.12"

name := "AdventOfCode2018"

addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.15" % Test
)
