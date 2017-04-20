

lazy val root = (project in file("."))
  .settings(
    organization := "com.github.agetakoyaki29",
    name := "lgeo",
    version := "0.6",
    scalaVersion := "2.11.8",

    libraryDependencies += scalactic,
    libraryDependencies += scalatest,

    logBuffered in Test := false
  )
  .dependsOn(mydouble)

// ---- lib ----
lazy val scalactic = "org.scalactic" %% "scalactic" % "2.2.5"
lazy val scalatest = "org.scalatest" %% "scalatest" % "2.2.5" % "test"

lazy val mydouble = RootProject(uri("git://github.com/agetakoyaki29/mydouble"))
