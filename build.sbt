import com.typesafe.sbt.SbtStartScript

name := "2014-ccg-supertagging"

version := "0.0.1"

scalaVersion := "2.11.1"

resolvers ++= Seq(
  "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots",
  "Sonatype Releases" at "https://oss.sonatype.org/content/repositories/releases",
  "OpenNLP repo" at "http://opennlp.sourceforge.net/maven2"
)

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.0.6",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.0",
  "org.slf4j" % "slf4j-log4j12" % "1.7.7",
  "org.apache.commons" % "commons-math3" % "3.2",
  "org.apache.opennlp" % "opennlp-tools" % "1.5.2-incubating",
  "junit" % "junit" % "4.11",
  "com.novocode" % "junit-interface" % "0.10" % "test")

seq(SbtStartScript.startScriptForClassesSettings: _*)

SbtStartScript.stage in Compile := Unit

scalacOptions ++= Seq("-deprecation", "-feature")
