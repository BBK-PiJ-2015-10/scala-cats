name := "cats"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= {

  val catsVersion = "2.2.0"
  val scalaTestVersion = "3.2.2"

  Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.scalatest" %% "scalatest" % scalaTestVersion % Test
  )

}


