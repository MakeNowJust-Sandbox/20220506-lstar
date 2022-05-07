Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "lstar",
    console / initialCommands := "import codes.quine.labs.lstar._",
    libraryDependencies += "org.scalameta" %% "munit" % "1.0.0-M3" % Test
  )
