import com.typesafe.sbt.pgp.PgpKeys

// WORKAROUND https://github.com/rtimush/sbt-updates/issues/10
addCommandAlias("dependencyUpdatesProject", "; reload plugins; dependencyUpdates; reload return")

net.virtualvoid.sbt.graph.DependencyGraphSettings.graphSettings

PgpKeys.useGpgAgent := true

cancelable in Global := true

// for 2.0-graph
//EnsimeKeys.ensimeJavaFlags += "-XX:MaxDirectMemorySize=4g"
