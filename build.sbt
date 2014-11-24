name := "xml"

scalaVersion := "2.11.4"

scalacOptions += "-deprecation"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"

scalacOptions += "-Xplugin:/home/swachter/.ivy/local/demo/boxer_2.11/0.1-SNAPSHOT/jars/boxer_2.11.jar"

// scalacOptions += "-Xshow-phases"

val extractXsdTestCollection = taskKey[Seq[File]]("extracts the files from the XSD test collection archive")

extractXsdTestCollection := {
  val f = (resourceManaged in Test).value
  f.mkdirs()
  Process("tar", List("-zxf", (baseDirectory.value / "src" / "test" / "data" / "xsts-2007-06-20.tar.gz").getCanonicalPath, "-C", f.getCanonicalPath)).run()
  println(s"extracting into: $f")
  Seq(f)
}

resourceGenerators in Test <+= extractXsdTestCollection

fullClasspath in Test += (resourceManaged in Test).value
