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
  "org.jfree" % "jfreechart" % "1.0.17",
  "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.0",
  "org.slf4j" % "slf4j-log4j12" % "1.7.7",
  "org.scalanlp" % "breeze_2.11" % "0.8",
  "org.scalanlp" % "breeze-natives_2.11" % "0.8",
  "org.apache.commons" % "commons-math3" % "3.2",
  "org.apache.opennlp" % "opennlp-tools" % "1.5.2-incubating",
  "org.scalanlp" % "junto" % "1.6.0"
	  exclude("com.typesafe.akka", "akka-actor_2.10")
	  exclude("com.typesafe", "scalalogging-log4j_2.10")
	  exclude("org.apache.logging.log4j", "log4j-core")
	  exclude("org.rogach", "scallop_2.10"),
  "org.jgrapht" % "jgrapht-jdk1.5" % "0.7.3",
  "org.abego.treelayout" % "org.abego.treelayout.netbeans" % "1.0.1" exclude("org.netbeans.api", "org-netbeans-api-visual"),
  "org.codeartisans.thirdparties.swing" % "org-netbeans-api-visual" % "2.23.1",
  "junit" % "junit" % "4.11",
  "com.novocode" % "junit-interface" % "0.10" % "test")

seq(SbtStartScript.startScriptForClassesSettings: _*)

SbtStartScript.stage in Compile := Unit

scalacOptions ++= Seq("-deprecation", "-feature")
