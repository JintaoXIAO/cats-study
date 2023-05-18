package jt

package parallel

object Example01 extends App {
  import cats.Semigroupal 
  import cats.instances.list._
  import cats.instances.either._ 
  import cats.syntax.parallel._
  type ErrorOr[A] = Either[Vector[String], A] 
  val error1: ErrorOr[Int] = Left(Vector("Error 1", "Error 3")) 
  val error2: ErrorOr[Int] = Left(Vector("Error 2"))
  val error3: ErrorOr[Int] = Right(10)
  val a = Semigroupal[ErrorOr].product(error1, error2)
  println(a)

  val b = (error1, error2, error3).parTupled  
  println(b)

  val c = (error1, error2, error3).parMapN(_ + _ + _)
  println(c)

  val d = (List(1,2,4), List(2,3,4,5)).parTupled
  println(d)

}
