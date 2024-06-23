name := "cats"

version := "0.1"

scalaVersion := "3.4.2"
semanticdbEnabled := true
semanticdbVersion := scalafixSemanticdb.revision
scalafixOnCompile := true

val catsVersion = "2.12.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion
)

scalacOptions ++= Seq(
  "-language:higherKinds",
  "-rewrite",
  "-Wunused:all",
  "--deprecation",
  "--explain",
  "--feature",
  "-Werror"
)
