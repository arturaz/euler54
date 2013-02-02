import sbt._
import sbt.Keys._

object Euler54Build extends Build {

  lazy val euler54 = Project(
    id = "euler54",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "Euler54",
      organization := "net.arturaz",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.10.0",
      scalacOptions := Seq("-feature")
      // add other settings here
    )
  )
}
