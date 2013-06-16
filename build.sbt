name := "SpiralS Mini"

version := "0.1"

organization := "ETHZ"

resolvers += ScalaToolsSnapshots

scalaOrganization := "org.scala-lang.virtualized"

scalaVersion := virtScala

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

scalacOptions += "-Yvirtualize"

//scalacOptions += "-Yvirtpatmat"

//scalacOptions in Compile ++= Seq(/*Unchecked, */Deprecation)

// needed for scala.tools, which is apparently not included in sbt's built in version
libraryDependencies += "org.scala-lang.virtualized" % "scala-library" % virtScala

libraryDependencies += "org.scala-lang.virtualized" % "scala-compiler" % virtScala

libraryDependencies += "org.scala-lang" % "scala-actors" % virtScala // for ScalaTest

libraryDependencies += scalaTest

libraryDependencies += bridj

libraryDependencies += "com.github.rwl" % "jtransforms" % "2.4.0"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "org.apache.commons" % "commons-math3" % "3.0"

libraryDependencies += "EPFL" % "lms_2.10" % "0.3-SNAPSHOT"

libraryDependencies += "org.slf4j" % "slf4j-nop" % "1.6.4"

libraryDependencies += "net.java.dev.jna" % "jna" % "3.4.0"

// tests are not thread safe
parallelExecution in Test := false

// disable publishing of main docs
publishArtifact in (Compile, packageDoc) := false

// continuations
autoCompilerPlugins := true

addCompilerPlugin("org.scala-lang.virtualized.plugins" % "continuations" % virtScala)


scalacOptions += "-P:continuations:enable"

fork in run := true


initialCommands in console := """
import ch.ethz.spirals.search.Search._
import ch.ethz.spirals.dsls._
import ch.ethz.spirals.util.Utilities._
import ch.ethz.spirals.rewrites._
println("  ___        _             _  ___   __  __  _        _ \r\n / __| _ __ (_) _ _  __ _ | |/ __| |  \\/  |(_) _ _  (_)\r\n \\__ \\| '_ \\| || '_|/ _` || |\\__ \\ | |\\/| || || ' \\ | |\r\n |___/| .__/|_||_|  \\__,_||_||___/ |_|  |_||_||_||_||_|\r\n      |_|                                              ");
// println("  ______       _             _   ______    _______ _       _ \n / _____)     (_)           | | / _____)  (_______|_)     (_)\n( (____  ____  _  ____ _____| |( (____     _  _  _ _ ____  _ \n \\____ \\|  _ \\| |/ ___|____ | | \\____ \\   | ||_|| | |  _ \\| |\n _____) ) |_| | | |   / ___ | | _____) )  | |   | | | | | | |\n(______/|  __/|_|_|   \\_____|\\_|______/   |_|   |_|_|_| |_|_|\n        |_|                                                  ")
println("https://github.com/GeorgOfenbeck/SpiralS_Mini")
"""
