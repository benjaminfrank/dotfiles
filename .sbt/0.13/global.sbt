import com.typesafe.sbt.pgp.PgpKeys._

// WORKAROUND https://github.com/rtimush/sbt-updates/issues/10
addCommandAlias("pluginDependencyUpdates", "; reload plugins; dependencyUpdates; reload return")

net.virtualvoid.sbt.graph.Plugin.graphSettings

useGpgAgent := true

cancelable in Global := true
