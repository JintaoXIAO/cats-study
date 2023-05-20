package jt

import cats.effect.IOApp
import cats.effect.IO
import utils._
import cats.syntax.apply._
import cats.syntax.parallel._
import cats.Parallel

object IOParallelism extends IOApp.Simple {
  
  val aniIO = IO(s"[${Thread.currentThread().getName}] Ani")
  val kamranIO = IO(s"[${Thread.currentThread().getName}] Kamran")

  val composedIO = for {
    ani <- aniIO
    kamran <- kamranIO  
  } yield s"$ani and $kamran love rock the jvm"

  val meaningOfLife: IO[Int] = IO.delay(43).myDebug
  val favLang: IO[String] = IO.delay("Scala").myDebug

  val goalInLife = (meaningOfLife, favLang) mapN { (n, s) => s"my goal in life is $n and $s" }

  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang)
  val goalInLifeParallel: IO.Par[String] = (parIO1, parIO2) mapN { (n, s) => s"my goal in life is $n and $s" }

  val goalInLife_v2: IO[String] = Parallel[IO].sequential(goalInLifeParallel)
  val goalInLife_v3: IO[String] = (meaningOfLife, favLang).parMapN{ (n, s) => s"my goal in life is $n and $s" }


  val aFailure: IO[String] = IO.raiseError(new RuntimeException("I can't do this!")).myDebug
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Second failure")).myDebug

  val parallelWithFailure: IO[String] = (meaningOfLife, aFailure) parMapN { _.toString + _ }  

  val twoFailures: IO[String] = (aFailure, anotherFailure) parMapN { _ + _ }

  val twoFailuresDelayed: IO[String] = (IO{Thread.sleep(400)} >> aFailure, anotherFailure) parMapN { _ + _ }

  def run: IO[Unit] = 
    // goalInLife.map(println)  
    // goalInLife_v2.void
    // goalInLife_v3.void
    // parallelWithFailure.void
    // twoFailures.void
    twoFailuresDelayed.void
    IO.print("")

}