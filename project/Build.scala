import sbt._
import java.io.File

object SpiralSBuild extends Build {

  // -DshowSuppressedErrors=false
  System.setProperty("showSuppressedErrors", "true")



  val scalaTest = "org.scalatest" % "scalatest_2.10" % "1.9.1" % "test"

  val virtScala = "2.10.2-RC1"

  val bridj = "com.nativelibs4java" % "bridj" % "0.6.1"



  //This is to integrade a build of LMS and a build of the roofline tool - both from github
  object V {
    val roofline_branch = "master"
    val perfplot_branch = "master"
    //val lms_branch = "wip-scala2.10"
    //val lms_branch = "delite-develop"
  }

  object Projects {
    lazy val perfplot = RootProject(uri("git://github.com/GeorgOfenbeck/perfplot.git#%s".format(V.perfplot_branch)))

  }


  lazy val spiralS = Project("SpiralS", file("."))
    //.dependsOn(Projects.perfplot)
}
