import org.scalajs.jsenv.nodejs.NodeJSEnv
// https://typelevel.org/sbt-typelevel/faq.html#what-is-a-base-version-anyway
ThisBuild / tlBaseVersion := "0.1" // your current series x.y

ThisBuild / organization := "dev.capslock"
ThisBuild / organizationName := "capslock.dev"
ThisBuild / startYear := Some(2025)
ThisBuild / licenses := Seq(License.MIT)
ThisBuild / developers := List(
  // your GitHub handle and name
  tlGitHubDev("windymelt", "windymelt")
)
ThisBuild / homepage := Some(url("https://github.com/windymelt/mcp-scala"))
ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/windymelt/mcp-scala"),
    "https://github.com/windymelt/mcp-scala.git"
  )
)

// publish website from this branch
ThisBuild / tlSitePublishBranch := Some("main")
ThisBuild / tlCiReleaseBranches := Seq()

val Scala3 = "3.6.3"
ThisBuild / crossScalaVersions := Seq(Scala3, "3.3.5")
ThisBuild / scalaVersion := Scala3 // the default Scala

lazy val root = tlCrossRootProject.aggregate(core)

val http4sVersion = "0.23.30"
val circeVersion = "0.14.12"
val fs2Version = "3.12.0"

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Full)
  .in(file("core"))
  .settings(
    name := "mcp-scala",
    version := "0.1.2",
    libraryDependencies ++= Seq(
      "org.typelevel" %%% "cats-core" % "2.13.0",
      "org.typelevel" %%% "cats-effect" % "3.6.0"
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
    libraryDependencies ++= Seq(
      "com.monovore" %%% "decline" % "2.5.0",
      "com.monovore" %%% "decline-effect" % "2.5.0"
    ),
    libraryDependencies += "org.scalactic" %%% "scalactic" % "3.2.19",
    libraryDependencies ++= Seq(
      "org.scalatest" %%% "scalatest" % "3.2.19" % "test",
      "org.typelevel" %%% "cats-effect-testing-scalatest" % "1.6.0" % "test"
    )
  )
  .jsSettings(
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    jsEnv := new NodeJSEnv(
      NodeJSEnv
        .Config()
        .withArgs(
          List("--experimental-require-module")
        )
    )
  )

lazy val example = project
  .in(file("example"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(core.js)
  .settings(
    name := "example",
    scalaJSUseMainModuleInitializer := true,
    scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.ESModule) },
    publish / skip := true
  )

lazy val docs = project.in(file("site")).enablePlugins(TypelevelSitePlugin)

// to use --experimental-require-module
ThisBuild / githubWorkflowJobSetup ++= Seq(
  WorkflowStep.Use(
    UseRef.Public("actions", "setup-node", "v3"),
    name = Some("Setup NodeJS v23"),
    params = Map("node-version" -> "23")
  )
)

// Publish to Maven Central
credentials += Credentials(
  "GnuPG Key ID",
  "gpg",
  "B588DDA3D3085F06DEA69CDF8ED2F321A52EEEA5", // key identifier
  "ignored" // this field is ignored; passwords are supplied by pinentry
)
