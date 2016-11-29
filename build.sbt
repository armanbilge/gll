name := "gll"
organization := "org.compevol"
version := "0.1"
scalaVersion := "2.11.8"
scalacOptions += "-optimize"
libraryDependencies += "org.spire-math" %% "spire" % "0.11.0"

publishMavenStyle := true
publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots")
  else
    Some("releases" at nexus + "service/local/staging/deploy/maven2")
}

homepage := Some(url("https://github.com/armanbilge/gll"))
licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
pomExtra :=
  <scm>
    <url>git@github.com:armanbilge/gll.git</url>
    <connection>scm:git@github.com:armanbilge/gll.git</connection>
  </scm>
    <developers>
      <developer>
        <id>armanbilge</id>
        <name>Arman Bilge</name>
        <url>http://armanbilge.com</url>
      </developer>
    </developers>
