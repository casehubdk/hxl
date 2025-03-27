val scala213Version = "2.13.12"
ThisBuild / scalaVersion := scala213Version
ThisBuild / crossScalaVersions := Seq(scala213Version, "3.3.0")
ThisBuild / organization := "io.github.casehubdk"
ThisBuild / organizationName := "CaseHubDK"

ThisBuild / tlBaseVersion := "0.3"
ThisBuild / sonatypeCredentialHost := xerial.sbt.Sonatype.sonatype01
//ThisBuild / tlFatalWarnings := true

ThisBuild / tlCiMimaBinaryIssueCheck := false
ThisBuild / tlMimaPreviousVersions := Set.empty
ThisBuild / mimaReportSignatureProblems := false
ThisBuild / mimaFailOnProblem := false
ThisBuild / mimaPreviousArtifacts := Set.empty

ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  Developer("valdemargr", "Valdemar Grange", "randomvald0069@gmail.com", url("https://github.com/valdemargr"))
)
ThisBuild / headerLicense := Some(HeaderLicense.Custom("Copyright (c) 2023 CaseHub DK"))

ThisBuild / startYear := Some(2024)

lazy val sharedSettings = Seq(
  libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % "2.9.0",
    "org.typelevel" %% "cats-free" % "2.9.0",
    "org.scalameta" %% "munit" % "1.0.0-M6" % Test,
    "org.typelevel" %% "munit-cats-effect" % "2.0.0-M3" % Test
  )
)

lazy val core = project
  .in(file("modules/core"))
  .settings(sharedSettings)
  .settings(name := "hxl")

lazy val natchez = project
  .in(file("modules/natchez"))
  .dependsOn(core)
  .settings(sharedSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "natchez-core" % "0.3.2",
      "org.tpolecat" %% "natchez-noop" % "0.3.2" % Test,
      "org.tpolecat" %% "natchez-testkit" % "0.3.2" % Test
    )
  )
  .settings(name := "hxl-natchez")
