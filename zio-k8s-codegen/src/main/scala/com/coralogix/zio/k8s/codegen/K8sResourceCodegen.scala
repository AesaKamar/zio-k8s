package com.coralogix.zio.k8s.codegen

import cats.data.{ EitherT, OptionT }
import cats.effect.IO
import com.coralogix.zio.k8s.codegen.internal.CodegenIO.*
import com.coralogix.zio.k8s.codegen.internal.Conversions.*
import com.coralogix.zio.k8s.codegen.internal.*
import fs2.io.file.Path
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.oas.models.OpenAPI
import io.swagger.v3.parser.core.models.{ ParseOptions, SwaggerParseResult }
import org.scalafmt.interfaces.Scalafmt

import java.io.File
import java.nio.charset.StandardCharsets
import scala.collection.JavaConverters._

class K8sResourceCodegen(val logger: sbt.Logger, val scalaVersion: String)
    extends Common with ModelGenerator with ClientModuleGenerator with SubresourceClientGenerator
    with UnifiedClientModuleGenerator {
  import cats.syntax.all._
  import scala.util.chaining._
  import cats.effect.instances._

  def generateAll(from: Path, targetDir: Path) = {
    for {
      // Loading
      spec         <- loadK8sSwagger(from).pipe(EitherT.apply[IO, Throwable, OpenAPI])
      scalafmt     <- IO.delay(Scalafmt.create(this.getClass.getClassLoader))
                        .pipe(EitherT.liftF[IO, Throwable, Scalafmt])

      // Identifying
      definitions   = spec.getComponents.getSchemas.asScala
                        .flatMap((IdentifiedSchema.identifyDefinition _).tupled)
                        .toSet
      definitionMap = definitions.map(d => d.name -> d).toMap

      paths             = spec.getPaths.asScala.flatMap((IdentifiedPath.identifyPath _).tupled).toList
      identified        = paths.collect { case i: IdentifiedAction => i }
      unidentified      = paths.filter {
                            case _: IdentifiedAction => false
                            case _                   => true
                          }
      _                <- checkUnidentifiedPaths(unidentified)
                            .pipe(EitherT.liftF[IO, Throwable, Unit])

      // Classifying
      resources        <- ClassifiedResource
                            .classifyActions(logger, definitionMap, identified.toSet)
                            .pipe(EitherT.liftF[IO, Throwable, Set[SupportedResource]])
      subresources      = resources.flatMap(_.subresources)
      subresourceIds    = subresources.map(_.id)
      subresourcePaths <- generateSubresourceAliases(scalafmt, targetDir, subresourceIds)
                            .pipe(EitherT.liftF[IO, Throwable, Set[Path]])

      // Generating code
      packagePaths     <- generateAllPackages(scalafmt, targetDir, definitionMap, resources)
                            .pipe(EitherT.liftF[IO, Throwable, Set[Path]])
      modelPaths       <- generateAllModels(scalafmt, targetDir, definitionMap, resources)
                            .pipe(EitherT.liftF[IO, Throwable, Set[Path]])
      unifiedPaths     <- generateUnifiedClientModule(
                            scalafmt,
                            targetDir,
                            clientRoot.mkString("."),
                            definitionMap,
                            resources
                          )
                            .pipe(EitherT.liftF[IO, Throwable, Set[Path]])
    } yield (packagePaths union modelPaths union subresourcePaths union unifiedPaths)
      .map(_.toNioPath.toFile)
      .toSeq
  }.value

  private def loadK8sSwagger(from: Path): IO[Either[Throwable, OpenAPI]] =
    IO.delay(logger.info("Loading k8s-swagger.json")) *>
      fs2.io.file
        .Files[IO]
        .readAll(from)
        .through(fs2.text.utf8.decode)
        .compile
        .string
        .flatMap { rawJson =>
          IO {
            val parser: OpenAPIParser = new OpenAPIParser
            val opts: ParseOptions = new ParseOptions()
            opts.setResolve(true)
            val parserResult: SwaggerParseResult =
              parser.readContents(rawJson, List.empty.asJava, opts)

            Option(parserResult.getMessages).foreach { messages =>
              messages.asScala.foreach(println)
            }

            Option(parserResult.getOpenAPI)
              .toRight(new RuntimeException(s"Failed to parse k8s swagger specs"))
          }
        }
        .recover { case t => t.asLeft }

  private val clientRoot = Vector("com", "coralogix", "zio", "k8s", "client")

  def generateAllPackages(
    scalafmt: Scalafmt,
    targetRoot: Path,
    definitionMap: Map[String, IdentifiedSchema],
    resources: Set[SupportedResource]
  ): IO[Set[Path]] =
    resources.toList
      .parTraverse(generatePackage(scalafmt, targetRoot, definitionMap, _))
      .map(_.toSet)

  private def generatePackage(
    scalafmt: Scalafmt,
    targetRoot: Path,
    definitionMap: Map[String, IdentifiedSchema],
    resource: SupportedResource
  ): IO[Path] =
    for {
      _ <- IO.delay(logger.info(s"Generating package code for ${resource.id}"))

      groupName = groupNameToPackageName(resource.gvk.group)
      pkg       = (clientRoot ++ groupName) :+ resource.gvk.version :+ resource.plural

      (entityPkg, entity) = splitName(resource.modelName)
      deleteResponse      = resource.actions
                              .map(_.endpointType)
                              .collectFirst { case EndpointType.Delete(_, _, responseTypeRef) =>
                                s"com.coralogix.zio.k8s.model.$responseTypeRef"
                              }
                              .getOrElse("com.coralogix.zio.k8s.model.pkg.apis.meta.v1.Status")

      src       <- generateModuleCode(
                     basePackageName = clientRoot.mkString("."),
                     modelPackageName = "com.coralogix.zio.k8s.model." + entityPkg.mkString("."),
                     name = resource.plural,
                     entity = entity,
                     statusEntity = findStatusEntity(definitionMap, resource.modelName).map(s =>
                       s"com.coralogix.zio.k8s.model.$s"
                     ),
                     deleteResponse = deleteResponse,
                     gvk = resource.gvk,
                     isNamespaced = resource.namespaced,
                     subresources = resource.subresources.map(_.id),
                     None,
                     resource.supportsDeleteMany
                   )
      targetDir  = pkg.foldLeft(targetRoot)(_ / _)
      _         <- fs2.io.file.Files[IO].createDirectories(targetDir)
      targetPath = targetDir / "package.scala"
      _         <- writeTextFile(targetPath, src)
      _         <- format(scalafmt, targetPath)
    } yield targetPath

  private def checkUnidentifiedPaths(
    paths: Seq[IdentifiedPath]
  ): IO[Unit] =
    for {
      whitelistInfo <- paths.toVector
                         .traverse { path =>
                           Whitelist.isWhitelistedPath(path) match {
                             case s @ Some(_) => IO.delay(s)
                             case None        =>
                               IO
                                 .delay(
                                   logger.error(s"Unsupported, non-whitelisted path: ${path.name}")
                                 )
                                 .as(None)
                           }
                         }
      issues         = whitelistInfo.collect { case Some(issueRef) => issueRef }.toSet
      _             <- if (whitelistInfo.contains(None)) {
                         IO(
                           throw new sbt.MessageOnlyException(
                             "Unknown, non-whitelisted path found. See the code generation log."
                           )
                         )
                       } else IO.unit
      _             <- IO.delay(logger.info(s"Issues for currently unsupported paths:"))
      _             <- issues.toVector
                         .traverse(issue => IO.delay(logger.info(s" - ${issue.url}")))
    } yield ()
}
