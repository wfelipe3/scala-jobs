package scalajobs.foundations

import cats.effect.IOApp
import cats.effect.IO
import java.util.UUID
import cats.implicits.*
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.dsl.impl.OptionalValidatingQueryParamDecoderMatcher
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import cats.Monad
import org.http4s.ember.server.EmberServerBuilder
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.circe.*

object Http4s extends IOApp.Simple:

  type Student = String
  case class Instructor(firstName: String, lastName: String)
  case class Course(
      id: String,
      title: String,
      year: Int,
      students: List[Student],
      instructorName: String
  )

  object CourseRepository:
    val catsEffectCourse = Course(
      "ACFBCF6B-922D-4329-9708-F3E5303F829E",
      "Rock the JVM Ultimate scala course",
      2022,
      List("Alice", "Bob", "Charlie"),
      "Martin"
    )

    val courses: Map[String, Course] = Map(catsEffectCourse.id -> catsEffectCourse)

    def findCourseById(courseId: UUID): Option[Course] =
      courses.get(courseId.toString)

    def findCoursesByInstructor(name: String): List[Course] =
      courses.values.filter(_.instructorName === name).toList
  end CourseRepository

  object InstructorQueryParamMatcher extends QueryParamDecoderMatcher[String]("instructor")
  object YearQueryParamMatcher       extends OptionalValidatingQueryParamDecoderMatcher[Int]("year")

  def courseRoutes[F[_]: Monad]: HttpRoutes[F] =
    val dsl = Http4sDsl[F]
    import dsl.*

    HttpRoutes.of[F] {
      case GET -> Root / "courses" :? InstructorQueryParamMatcher(
            instructor
          ) +& YearQueryParamMatcher(maybeYear) =>
        val courses = CourseRepository.findCoursesByInstructor(instructor)
        maybeYear match
          case Some(year) =>
            year.fold(
              _ => BadRequest("Year is not a number"),
              year => Ok(courses.filter(_.year === year).asJson)
            )
          case None => Ok(courses.asJson)

      case GET -> Root / "courses" / UUIDVar(courseId) / "students" =>
        CourseRepository.findCourseById(courseId).map(_.students) match
          case Some(students) =>
            Ok(students.asJson)
          case None => NotFound(s"No course with $courseId found")
    }

  def run =
    EmberServerBuilder
      .default[IO]
      .withHttpApp(courseRoutes[IO].orNotFound)
      .build
      .use(_ => IO.println("Server started on port 8080") *> IO.never)
