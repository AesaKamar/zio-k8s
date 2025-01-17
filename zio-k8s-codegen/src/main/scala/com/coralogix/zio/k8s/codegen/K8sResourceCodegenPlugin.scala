package com.coralogix.zio.k8s.codegen

import sbt.Keys.*
import sbt.*

import scala.sys.process.*
import K8sSwaggerPlugin.autoImport.*
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.global

object K8sResourceCodegenPlugin extends AutoPlugin {
  object autoImport {
    lazy val generateSources =
      Def.task {
        val log = streams.value.log
        val runtime = zio.Runtime.default
        val scalaVer = scalaVersion.value
        val codegen = new K8sResourceCodegen(log, scalaVer)

        val sourcesDir = (Compile / sourceManaged).value
        val ver = scalaVersion.value

        val cachedFun = FileFunction.cached(
          streams.value.cacheDirectory / s"k8s-src-$ver",
          FileInfo.hash
        ) { input: Set[File] =>
          input.foldLeft(Set.empty[File]) { (result, k8sSwagger) =>

            val fs =
              codegen
                .generateAll(
                  fs2.io.file.Path.fromNioPath(k8sSwagger.toPath),
                  fs2.io.file.Path.fromNioPath(sourcesDir.toPath)
                )
                .unsafeRunSync
                .left
                .map(throw _)
                .toOption
                .get

            result union fs.toSet
          }
        }

        val k8sSwagger = getK8sSwagger.value

        cachedFun(Set(k8sSwagger)).toSeq
      }
  }

  import autoImport._

  override val requires = K8sSwaggerPlugin

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      Compile / sourceGenerators += generateSources.taskValue
    )
}
