package scalajobs.foundations

import cats.implicits.*
import cats.Functor
import cats.Applicative
import cats.ApplicativeError

object Cats:

  private def incrementer[F[_]: Functor](container: F[Int]): F[Int] =
    container.map(_ + 1)

  def main(args: Array[String]): Unit =
    val listFunctor = Functor[List]
    val otherList   = listFunctor.map(List(1, 2, 3))(_ + 1)

    type ErrorOr[A] = Either[String, A]
    val applicativeEither = ApplicativeError[ErrorOr, String] 

    val listApplicative = Applicative[List]
    listApplicative.pure(12)

    val e = applicativeEither.raiseError("Error")

    println(otherList)
    println(incrementer(List(1, 2, 3)))
    println(12.pure[List])

end Cats
