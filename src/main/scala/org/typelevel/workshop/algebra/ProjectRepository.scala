package org.typelevel.workshop.algebra

import org.typelevel.workshop.model.Project

trait ProjectRepository[F[_]] {
  def findByName(name: String): F[Option[Project]]

  def allProjects(): F[List[Project]]

  def deleteProject(name: String): F[Unit]
}

object ProjectRepository {
  def apply[F[_]: ProjectRepository]: ProjectRepository[F] = implicitly
}
