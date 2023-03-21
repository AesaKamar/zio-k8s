package com.coralogix.zio.k8s.codegen.internal

import cats.effect.IO
import com.coralogix.zio.k8s.codegen.internal.CodegenIO.{readTextFile, writeTextFile}
import com.coralogix.zio.k8s.codegen.internal.Conversions.splitName
import fs2.io.file.Path
import io.swagger.v3.oas.models.media.ObjectSchema
import org.scalafmt.interfaces.Scalafmt

import java.nio.file.{Path as JPath, Paths as JPaths}
import scala.meta.Tree
import scala.meta.internal.prettyprinters.TreeSyntax

trait Common {
  protected def findStatusEntity(
    definitions: Map[String, IdentifiedSchema],
    modelName: String
  ): Option[String] = {
    val modelSchema = definitions(modelName).schema.asInstanceOf[ObjectSchema]
    findStatusEntityOfSchema(modelSchema)
  }

  protected def findStatusEntityOfSchema(modelSchema: ObjectSchema): Option[String] =
    for {
      properties       <- Option(modelSchema.getProperties)
      statusPropSchema <- Option(properties.get("status"))
      ref              <- Option(statusPropSchema.get$ref())
      (pkg, name)       = splitName(ref.drop("#/components/schemas/".length))
    } yield pkg.mkString(".") + "." + name

  def scalaVersion: String

  protected def prettyPrint(tree: Tree): String = {
    val dialect =
      if (scalaVersion.startsWith("3.")) scala.meta.dialects.Scala3
      else if (scalaVersion.startsWith("2.13.")) scala.meta.dialects.Scala213
      else scala.meta.dialects.Scala212
    val prettyprinter = TreeSyntax[Tree](dialect)
    prettyprinter(tree).toString
  }

  protected def format(scalafmt: Scalafmt, path: Path): IO[Path] =
    if (scalaVersion.startsWith("3."))
      IO.delay(path) // NOTE: no formatting for scala 3 yet
    else
      for {
        code     <- readTextFile(path)
        formatted = scalafmt.format(JPaths.get(".scalafmt.conf"), path.toNioPath, code)
        _        <- writeTextFile(path, formatted)
      } yield path
}
