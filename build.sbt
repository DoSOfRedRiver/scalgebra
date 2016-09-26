name := "ScalaAlgebra"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies <++= (scalaVersion)(sv =>
  Seq(
    "org.scala-lang" % "scala-reflect" % "2.11.7",
    "org.scala-lang" % "scala-compiler" % "2.11.7"
  )
)
