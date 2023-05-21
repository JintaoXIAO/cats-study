package jt

import cats.syntax.either._
import cats.syntax.flatMap._
import cats.effect.IOApp
import cats.effect.IO
import scala.concurrent.duration._
import utils._
import cats.effect.kernel.Outcome
import cats.effect.kernel.Fiber
import cats.effect.kernel.Outcome.Succeeded
import cats.syntax.flatMap

object RacingIOs extends IOApp.Simple {

  def runWithSleep[A](value: A, duration: FiniteDuration): IO[A] = 
    (
      IO(s"starting computation: $value").myDebug >>
      IO.sleep(duration) >>
      IO(s"computation for $value") >>
      IO(value)       
    ).onCancel(IO(s"computation CANCELLED for $value").myDebug.void)

  def testRace() = {
    val meaningOfLife = runWithSleep(42, 1.second)  
    val favLang = runWithSleep("Scala", 2.seconds)
    val first: IO[Either[Int, String]] = IO.race(meaningOfLife, favLang)    

    first.flatMap {
      case Left(value) => IO(s"Meaning of life: $value")  
      case Right(value) => IO(s"Fav language: $value")
    }    
  }

  def testRacePair() = {
    val meaningOfLife = runWithSleep(42, 1.second)  
    val favLang = runWithSleep("Scala", 2.seconds)

    val raceResult: IO[Either[
      (Outcome[IO, Throwable, Int], Fiber[IO, Throwable, String]), 
      (Fiber[IO, Throwable, Int], Outcome[IO, Throwable, String])]] = 
        IO.racePair(meaningOfLife, favLang)        

    raceResult.flatMap {
      case Left((outMol, fibLang)) => fibLang.cancel >> IO("MOL won").myDebug >> IO(outMol).myDebug
      case Right((fibMol, outLang)) => fibMol.cancel >> IO("Lang won").myDebug >> IO(outLang).myDebug
    }
  }  

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = 
    IO.race(io, IO.sleep(duration) >> IO.raiseError(new RuntimeException("timeout"))) flatMap {
      case Left(value) => IO(value)
      case Right(err) => IO.raiseError(err)
    } 

  def unrace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] = 
    IO.racePair(ioa, iob) flatMap {
      case Left((_, fibB)) => 
        fibB.join flatMap {
          case Succeeded(fb) => fb.map(_.asRight[A])
          case _ => IO.raiseError(new RuntimeException("failed"))
        }
      case Right((fibA, _)) => 
        fibA.join flatMap {
          case Succeeded(fa) => fa map { _.asLeft[B] } 
          case _ => IO.raiseError(new RuntimeException("failed"))
        }  
    }  

  def simpleRace[A, B](ioa: IO[A], iob: IO[B]): IO[Either[A, B]] =   
    IO.racePair(ioa, iob) flatMap {
      case Left((outA, fibB)) => fibB.cancel >> { outA match {
        case Succeeded(fa) => (fa map (_.asLeft[B]))
        case _ => IO.raiseError(new RuntimeException("failed"))
      }}
      case Right((fibA, outB)) => fibA.cancel >> { outB match {
        case Succeeded(fb) => (fb map (_.asRight[A])) 
        case _ => IO.raiseError(new RuntimeException("failed"))
      }}
    }    

  override def run: IO[Unit] = 
    // testRace().myDebug.void
    testRacePair().myDebug.void
}