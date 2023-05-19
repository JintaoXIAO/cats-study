package jt

import cats.effect.IO
import cats.Monad
import cats.syntax.apply._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.syntax.semigroup._
import cats.syntax.semigroupal._
import scala.io.StdIn

object IOIntroduction {
  val ourFirstIO: IO[Int] = IO.pure(123)
  val outSecondIO: IO[Int] = 34.pure[IO]

  val aDelayedIO: IO[Int] = IO.delay{ 
    println("producing an integer")  
    123
  }

  val aDelayedIO2: IO[Int] = IO {
    println("producing an integer")  
    12
  }  

  val improvedMeaningOfLife = ourFirstIO.map{ _ * 2 }  
  val printedMeaningOfLife = ourFirstIO.flatMap{ IO.print(_) }
  val printedMeaningOfLife2 = ourFirstIO >>= IO.println

  def smallProgram(): IO[Unit] = for {
    line1 <- IO { StdIn.readLine() }
    line2 <- IO { StdIn.readLine() }
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  def smallProgram1(): IO[Unit] = 
    (Monad[IO].map2(IO { StdIn.readLine() }, IO { StdIn.readLine()} ){ _ |+| _ }) >>= IO.println

  def combineMeaningOfLife: IO[Int] = (ourFirstIO, improvedMeaningOfLife) mapN { _ + _ }    

  def sequenceTakeLast[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa flatMap { _ => iob }

  def sequenceTakeLast_v1[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa *> iob

  def sequenceTakeLast_v2[A, B](ioa: IO[A], iob: IO[B]): IO[B] = ioa >> iob  

  def sequenceTakeFirst[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa flatMap { a => iob map { _ => a } }

  def sequenceTakeFirst_v1[A, B](ioa: IO[A], iob: IO[B]): IO[A] = ioa <* iob

  def forever[A](io: IO[A]): IO[A] = io >>= (_ => forever(io))

  def forever_v1[A](io: IO[A]): IO[A] = io >> forever_v1(io)

  def forever_v2[A](io: IO[A]): IO[A] = io *> forever_v2(io)  

  def forever_v3[A](io: IO[A]): IO[A] = io.foreverM

  def convert[A, B](ioa: IO[A], value: B): IO[B] = ioa map { _ => value }  

  def convert_v1[A, B](ioa: IO[A], value: B): IO[B] = ioa.as(value)

  def asUnit[A](ioa: IO[A]): IO[Unit] = convert(ioa, ())

  def sum(n: Int): Int = 
    if (n == 0) 0
    else n + sum(n - 1)   

  def sumIO(n: Int): IO[Int] = 
    if (n == 0) IO.pure(0)
    else for {
      a <- IO.pure(n)
      b <- sumIO(n - 1)  
    } yield a + b   

  def fibonacci(n: Int): BigInt = 
    if (n == 0) 0
    else if (n == 1) 1
    else fibonacci(n-1) + fibonacci(n-2) 

  def fibonacciIO(n: Int): IO[BigInt] = 
    if (n == 0) IO.pure(0) 
    else if (n == 1) IO.pure(1)
    else (fibonacciIO(n-1), fibonacciIO(n-2)) mapN { _ + _ }

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global  
    
    // (sequenceTakeLast(IO{ println("a"); 1}, IO{ println("b"); 2}) >>= IO.println).unsafeRunSync()
    // (sequenceTakeFirst(IO{ println("a"); 1}, IO{ println("b"); 2}) >>= IO.println).unsafeRunSync()
    // forever(IO { StdIn.readLine() }).unsafeRunSync()
    // println(convert(IO.pure(123), "hello").unsafeRunSync())
    // sum(100000) //stackoverflow
    // (sumIO(100000) >>= IO.println).unsafeRunSync()
    // fib: 0 1 1 2 3 5
    // println(fibonacci(10))  
    println(fibonacciIO(10).unsafeRunSync())
  }
}