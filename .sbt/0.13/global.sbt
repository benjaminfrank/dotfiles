import com.typesafe.sbt.pgp.PgpKeys

// WORKAROUND https://github.com/rtimush/sbt-updates/issues/10
addCommandAlias("pluginDependencyUpdates", "; reload plugins; dependencyUpdates; reload return")

net.virtualvoid.sbt.graph.Plugin.graphSettings

PgpKeys.useGpgAgent := true

cancelable in Global := true
