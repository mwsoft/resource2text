name := """resource2text"""

version := "1.0"

scalaVersion := "2.10.2"

resolvers += "Atilika Open Source repository" at "http://www.atilika.org/nexus/content/repositories/atilika"

libraryDependencies ++= Seq(
  "commons-cli" % "commons-cli" % "1.2",
  "commons-io" % "commons-io" % "2.4",
  "joda-time" % "joda-time" % "2.3",
  "org.atilika.kuromoji" % "kuromoji" % "0.7.7",
  "org.joda" % "joda-convert" % "1.5",
  "org.apache.commons" % "commons-compress" % "1.6",
  "junit" % "junit" % "4.11" % "test",
  "org.scalatest" %% "scalatest" % "2.0.M5b" % "test"
)

EclipseKeys.createSrc := EclipseCreateSrc.Default + EclipseCreateSrc.Resource

EclipseKeys.withSource := true
