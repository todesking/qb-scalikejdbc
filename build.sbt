name := "qb-scalikejdbc"

version := "0.0.0"

organization := "com.todesking"

scalaVersion := "2.10.4"

libraryDependencies ++= Seq(
  "org.scalikejdbc" %% "scalikejdbc"               % "1.7.7",
  "org.scalikejdbc" %% "scalikejdbc-interpolation" % "1.7.7",
  "org.specs2" %% "specs2-core" % "2.4.5" % "test",
  "com.h2database" % "h2" % "1.3.176" % "test"
)

resolvers += "linter" at "http://hairyfotr.github.io/linteRepo/releases"

addCompilerPlugin("com.foursquare.lint" %% "linter" % "0.1-SNAPSHOT")

scalacOptions ++= Seq("-deprecation", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")
