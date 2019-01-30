package org.typelevel.workshop.algebra

import cats.effect.IO
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

trait Logging[F[_]] {
  def logInfo(s: String): F[Unit]
  def logError(s: String): F[Unit]
}

object Logging {
  def apply[F[_]: Logging]: Logging[F] = implicitly

  implicit def loggingIO: Logging[IO] = new Logging[IO] {

    val logger = Slf4jLogger.create[IO]

    def logInfo(s: String): IO[Unit] = logger.flatMap(_.info(s))

    def logError(s: String): IO[Unit] = logger.flatMap(_.error(s))
  }
}
