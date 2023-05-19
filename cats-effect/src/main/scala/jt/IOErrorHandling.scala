package jt

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import scala.util.Try
import scala.util.Failure
import scala.util.Success

object IOErrorHandling {

  val aFailedComputation: IO[Unit] = IO { throw new RuntimeException("A Failure") }  

  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail"))  

  val dealWithIt = aFailure.handleErrorWith { 
    case _: RuntimeException => IO.delay{ println("I'm still here") }
  }

  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  val resultAsString: IO[String] = aFailure.redeem( ex => s"Fail: $ex", value => s"Success: $value")

  val resultAsEffect: IO[Unit] = aFailure.redeemWith(ex => IO.println(s"Fail: $ex"), value => IO.println(s"Success: $value"))

  def option2IO[A](option: Option[A])(ifEmpty: Throwable): IO[A] = option match {
    case None => IO.raiseError(ifEmpty)  
    case Some(value) => IO.pure(value)
  }

  def try2IO[A](aTry: Try[A]): IO[A] = aTry match {
    case Failure(exception) => IO.raiseError(exception)  
    case Success(value) => IO.pure(value)
  }

  def either2IO[A](anEither: Either[Throwable, A]): IO[A] = anEither match {
    case Left(ex) => IO.raiseError(ex)  
    case Right(value) => IO.pure(value)
  }

  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = 
    io.redeemWith(handler, IO.pure)
  
  def main(args: Array[String]): Unit = {
    // aFailedComputation.unsafeRunSync()
    // aFailure.unsafeRunSync()
    // dealWithIt.unsafeRunSync()
    // println(resultAsString.unsafeRunSync())
    resultAsEffect.unsafeRunSync()
  }

}