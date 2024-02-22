package scalajobs.foundations

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.MonadCancelThrow
import doobie.util.transactor.Transactor
import doobie.implicits.*

object Doobie extends IOApp.Simple:

  case class Student(id: Int, name: String)

  val xa: Transactor[IO] = Transactor.fromDriverManager[IO](
    "org.postgresql.Driver",
    "jdbc:postgresql://localhost:5432/demo",
    "docker",
    "docker"
  )

  def findAllStudentsNames: IO[List[String]] =
    sql"SELECT name FROM students"
      .query[String]
      .to[List]
      .transact(xa)

  def saveStudent(id: Int, name: String): IO[Int] =
    sql"INSERT INTO students (id, name) VALUES ($id, $name)".update.run
      .transact(xa)

  def findByInitialLetter(letter: String): IO[List[Student]] =
    val select = fr"SELECT id, name"
    val from   = fr"FROM students"
    val where  = fr"WHERE left(name, 1) = $letter"
    (select ++ from ++ where)
      .query[Student]
      .to[List]
      .transact(xa)

  trait Students[F[_]]:
    def findById(id: Int): F[Option[Student]]
    def findAll: F[List[Student]]
    def create(name: String): F[Int]
    def findAllStudentsNames: F[List[String]]

  object Students:
    def make[F[_]: MonadCancelThrow](xa: Transactor[F]): Students[F] =
      new Students[F] {
        def findById(id: Int): F[Option[Student]] =
          sql"SELECT id, name FROM students WHERE id = $id"
            .query[Student]
            .option
            .transact(xa)
        def findAll: F[List[Student]] =
          sql"SELECT id, name FROM students"
            .query[Student]
            .to[List]
            .transact(xa)
        def create(name: String): F[Int] =
          sql"INSERT INTO students (name) VALUES ($name)".update
            .withUniqueGeneratedKeys[Int]("id")
            .transact(xa)
        def findAllStudentsNames: F[List[String]] =
          sql"SELECT name FROM students"
            .query[String]
            .to[List]
            .transact(xa)
      }

  override def run: IO[Unit] =
    val students = Students.make(xa)
    for
      _ <- students.create("felipe")
      _ <- students.findAll.map(println)
      _ <- findByInitialLetter("f").map(println)
    yield ()
