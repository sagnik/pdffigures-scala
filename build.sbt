
organization := "edu.psu.ist.sagnik.research"

name := "pdffigures"

version := "0.0.1"


javacOptions += "-Xlint:unchecked"

scalaVersion := "2.11.7"

crossScalaVersions := Seq("2.11.7", "2.10.5")
  

resolvers ++= Seq(
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases/",
  "Sonatype Shapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
  "JAI releases" at "http://maven.geotoolkit.org/"
)


libraryDependencies ++= Seq(
   //jackson for json
  "org.json4s" %% "json4s-native" % "3.2.11",
  "org.json4s" %% "json4s-jackson" % "3.2.10",

  // pdf parsing libraries
  "org.apache.pdfbox"    %  "pdfbox"          %  "1.8.7",
  "org.apache.tika"      %  "tika-bundle"     %  "1.6",
 // avro codegen
  "com.chuusai"          %% "shapeless"       % "2.2.3",
  // testing
  "org.scalatest"        %% "scalatest"  %  "2.2.4",
  //log4j
  "log4j" % "log4j" % "1.2.15" excludeAll(
    ExclusionRule(organization = "com.sun.jdmk"),
    ExclusionRule(organization = "com.sun.jmx"),
    ExclusionRule(organization = "javax.jms")
    )
  )

libraryDependencies += "javax.media" % "jai_core" % "1.1.3"

libraryDependencies += "commons-collections" % "commons-collections" % "3.2.1"

//libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.4"
// for iterator

javacOptions ++= Seq("-source", "1.6", "-target", "1.6")

//scalacOptions += "-target:jvm-1.6"

fork := true

testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")

testOptions in Test += Tests.Argument("-oF")

fork in Test := false

parallelExecution in Test := false
