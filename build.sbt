name := "qb-scalikejdbc"

version := "0.0.0"

organization := "com.todesking"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "2.4.5" % "test"
)

resolvers += "linter" at "http://hairyfotr.github.io/linteRepo/releases"

addCompilerPlugin("com.foursquare.lint" %% "linter" % "0.1-SNAPSHOT")

scalacOptions ++= Seq("-deprecation", "-feature")

scalacOptions in Test ++= Seq("-Yrangepos")
