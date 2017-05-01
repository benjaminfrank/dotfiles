import org.ensime.EnsimeKeys._
import org.ensime.EnsimeCoursierKeys._

// WORKAROUND https://github.com/rtimush/sbt-updates/issues/10
addCommandAlias("dependencyUpdatesProject", "; reload plugins; dependencyUpdates; reload return")

net.virtualvoid.sbt.graph.DependencyGraphSettings.graphSettings

cancelable in Global := true

// for 2.0-graph
ensimeIgnoreMissingDirectories in ThisBuild := true
ensimeJavaFlags += "-Xmx4g"

// causes weird problems with clean
//historyPath := Some((baseDirectory in ThisBuild).value / ".history")
