package com.coralogix.zio.k8s.codegen.internal

import cats.effect.IO
import fs2.Chunk
import fs2.io.file.Path

import java.nio.charset.StandardCharsets

object CodegenIO {

  def readTextFile(path: Path): IO[String] =
    fs2.io.file.Files[IO]
      .readAll(path)
      .through(fs2.text.utf8.decode)
      .compile
      .string

  def writeTextFile(path: Path, contents: String): IO[Unit] =
    fs2.Stream.
      chunk(Chunk.array(contents.getBytes(StandardCharsets.UTF_8)))
      .through(fs2.io.file.Files[IO].writeAll(path))
      .compile
      .drain

}