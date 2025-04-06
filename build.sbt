// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.0" // your current series x.y

ThisBuild / organization := "dev.capslock"
ThisBuild / organizationName := "capslock.dev"
ThisBuild / startYear := Some(2025)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("windymelt", "windymelt")
)

// publish to s01.oss.sonatype.org (set to true to publish to oss.sonatype.org instead)
ThisBuild / tlSonatypeUseLegacyHost := false

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")

val Scala3 = "3.6.3"
ThisBuild / crossScalaVersions := Seq(Scala3, "3.3.5")
ThisBuild / scalaVersion := Scala3 // the default Scala

lazy val root = tlCrossRootProject.aggregate(core)

val http4sVersion = "0.23.30"
val circeVersion = "0.14.12"
val fs2Version = "3.12.0"

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .in(file("core"))
  .settings(
    name := "mcp-scala",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "org.typelevel" %%% "cats-effect" % "3.6.0",
      "org.scalameta" %%% "munit" % "1.1.0" % Test,
      "org.typelevel" %%% "munit-cats-effect" % "2.1.0" % Test
    ),
    libraryDependencies ++= Seq(
      "org.http4s" %%% "http4s-ember-client" % http4sVersion,
      "org.http4s" %%% "http4s-ember-server" % http4sVersion,
      "org.http4s" %%% "http4s-dsl" % http4sVersion
    ),
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core",
      "io.circe" %%% "circe-generic",
      "io.circe" %%% "circe-parser"
    ).map(_ % circeVersion),
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-core" % fs2Version,
      "co.fs2" %%% "fs2-io" % fs2Version
    ),
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %%% "tapir-core" % "1.11.23",
      "com.softwaremill.sttp.tapir" %%% "tapir-apispec-docs" % "1.11.23",
      "com.softwaremill.sttp.apispec" %%% "openapi-circe" % "0.11.7",
      "com.softwaremill.sttp.apispec" %%% "jsonschema-circe" % "0.11.7"
    ),
    libraryDependencies += "org.scalactic" %%% "scalactic" % "3.2.19",
    libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) },
    Compile / mainClass := Some("dev.capslock.mcpscala.StdioMain")
  )

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin)
