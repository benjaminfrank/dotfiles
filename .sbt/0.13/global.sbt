import org.ensime.EnsimeKeys._
import org.ensime.EnsimeCoursierKeys._

// WORKAROUND https://github.com/rtimush/sbt-updates/issues/10
addCommandAlias("dependencyUpdatesProject", "; reload plugins; dependencyUpdates; reload return")

net.virtualvoid.sbt.graph.DependencyGraphSettings.graphSettings

cancelable in Global := true

// for 2.0-graph
//EnsimeKeys.ensimeJavaFlags += "-XX:MaxDirectMemorySize=4g"
ensimeJavaHome in ThisBuild := file("/usr/lib/jvm/java-8-openjdk")
ensimeServerVersion in ThisBuild := "2.0.0-SNAPSHOT"
