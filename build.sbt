import com.untyped.sbtless.Plugin.LessKeys
import com.untyped.sbtless.Plugin._
import sbt._

enablePlugins(ScalaJSPlugin)

name := "fast-links"

version := "0.1"

scalaVersion := "2.11.6"

scalacOptions := Seq(
  "-unchecked", "-deprecation",
  "-encoding", "utf8",
  "-Xelide-below", annotation.elidable.ALL.toString
)

persistLauncher := true

libraryDependencies ++= Seq(
  "com.lihaoyi" %%% "upickle" % "0.2.8",
  "io.github.widok" %%% "widok" % "0.2.1"
)

lessSettings

includeFilter in(Compile, LessKeys.less) := "index.less" | "font.css"

lazy val generate = taskKey[Unit]("generate")

generate := {
  (copyResources in Compile).value
  (LessKeys.less in Compile).value
  (fastOptJS in Compile).value
}
