package org.typelevel.workshop.http

import cats.effect._
import cats.implicits._
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s.HttpService
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl
import org.typelevel.workshop.algebra.{Logging, ProjectRepository}

class ProjectService[F[_]: Sync: ProjectRepository: Logging]
    extends Http4sDsl[F] {

  def service: HttpService[F] = HttpService[F] {

    case GET -> Root / name =>
      ProjectRepository[F].findByName(name).flatMap {
        case Some(project) =>
          Logging[F].logInfo(s"findByName:${name}") *> Ok(project)
        case None =>
          Logging[F].logError(s"findByName:${name}") *> NotFound(
            s"No project found: $name".asJson)
      }

    case GET -> Root =>
      ProjectRepository[F].allProjects().flatMap {
        case Nil =>
          Logging[F].logError(s"allProjects") *> NotFound(
            s"No projects found".asJson)
        case list =>
          Logging[F].logInfo(s"allProjects") *> Ok(list)
      }

    case req @ DELETE -> Root / name =>
      ProjectRepository[F].deleteProject(name).flatMap(_ => NoContent())

  }
}
