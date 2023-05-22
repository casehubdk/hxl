val scala213Version = "2.13.10"
val scala32Version = "3.2.2"

ThisBuild / scalaVersion := scala213Version
ThisBuild / crossScalaVersions := Seq(scala213Version, scala32Version)
ThisBuild / organization := "io.github.casehubdk"
ThisBuild / organizationName := "CaseHubDK"

ThisBuild / tlBaseVersion := "0.1"

ThisBuild / licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / developers := List(
  Developer("valdemargr", "Valdemar Grange", "randomvald0069@gmail.com", url("https://github.com/valdemargr"))
)

lazy val root = project.in(file("hxl"))
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.scalameta" %% "munit" % "1.0.0-M6" % Test,
      "org.typelevel" %% "munit-cats-effect" % "2.0.0-M3" % Test,
    )
  )
