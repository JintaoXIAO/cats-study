package jt

import scala.concurrent.duration._
import cats.syntax.flatMap._
import cats.syntax.apply._
import cats.effect.IOApp
import cats.effect.IO
import utils._
import cats.effect.kernel.Fiber
import cats.effect.kernel.Outcome
import cats.effect.kernel.Outcome.Succeeded
import cats.effect.kernel.Outcome.Errored
import cats.effect.kernel.Outcome.Canceled

object Fibers extends IOApp.Simple {

  val meaningOfLife = IO.pure(42)
  val favLang = IO.pure("Scala")  

  def sameThreadIOs() = for {
    _ <- meaningOfLife.myDebug
    _ <- favLang.myDebug 
  } yield ()

  def createFiber: Fiber[IO, Throwable, String] = ???

  val aFiber: IO[Fiber[IO, Throwable, Int]] = meaningOfLife.myDebug.start  

  def differentThreadIOs() = for {
    _ <- aFiber
    _ <- favLang.myDebug  
  } yield () 

  def runOnSomeOtherThread[A](io: IO[A]): IO[Outcome[IO, Throwable, A]] = for {
    fib <- io.start
    result <- fib.join  
  } yield result

  val someIOOnAnotherThread = runOnSomeOtherThread(meaningOfLife)
  val someResultFromAnotherThread = someIOOnAnotherThread.flatMap { 
    case Succeeded(effect) => effect
    case Errored(e) => IO(0) 
    case Canceled() => IO(0)
  }

  def throwOnAnotherThread() = for {
    fib <- IO.raiseError[Int](new RuntimeException("no number for you")).start
    result <- fib.join
  } yield result 

  def testCancel() = { 
    val task = IO("Starting").myDebug >> IO.sleep(1.second) >> IO("Done").myDebug
    val taskWithCancellationHandler = task.onCancel(IO("I'm being cancelling").myDebug.void)

    for {
      fib <- taskWithCancellationHandler.start
      _ <- IO.sleep(500.milliseconds) >> IO("cancelling").myDebug
      _ <- fib.cancel
      result <- fib.join
    } yield result
  }

  def processResultsFromFiber[A](io: IO[A]): IO[A] = {
    def outComeHandler: Outcome[IO, Throwable, A] => IO[A] = {
      case Succeeded(effect) => effect
      case Errored(e) => IO.raiseError(e)  
      case Canceled() => IO.raiseError(new RuntimeException("io has been cancelled"))  
    }  
    io.start >>= (_.join) >>= outComeHandler 
/*
    (for {
      fib <- io.myDebug.start  
      result <- fib.join
    } yield result) flatMap outComeHandler
*/    
  }

  def tupleIOs[A, B]()(ioa: IO[A], iob: IO[B]): IO[(A, B)] = {
    
    val tupleOutcome:  IO[(Outcome[IO, Throwable, A], Outcome[IO, Throwable, B])] = for {
      fib1 <- ioa.start
      fib2 <- iob.start
      rst1 <- fib1.join  
      rst2 <- fib2.join
    } yield (rst1, rst2) 

    tupleOutcome flatMap {
      case (Succeeded(eff1), Succeeded(eff2)) => (eff1, eff2) mapN ((_, _))
      case (Errored(err), _) => IO.raiseError(err)
      case (_, Errored(err)) => IO.raiseError(err)
      case _ => IO.raiseError(new RuntimeException("some computation cancelled"))
    }    
  }   

  def timeout[A](io: IO[A], duration: FiniteDuration): IO[A] = {
    val computation: IO[Outcome[IO, Throwable, A]] = for {
      fib <- io.start  
      _ <- IO.sleep(duration) >> fib.cancel
      result <- fib.join
    } yield result

    computation >>= {
      case Succeeded(effect) => effect
      case Canceled() => IO.raiseError(new RuntimeException("timeout"))
      case Errored(err) => IO.raiseError(err)
    }

  }

  def test1() = {
    val aComputation = IO("Starting").myDebug >> IO.sleep(1.second) >> IO("done!").myDebug >> IO(42)
    processResultsFromFiber(aComputation)
  }  


  override def run: IO[Unit] = 
    test1().void
    // testCancel().myDebug.void
    // sameThreadIOs()  
    // differentThreadIOs()
    // runOnSomeOtherThread(meaningOfLife.myDebug).myDebug.void
    // throwOnAnotherThread().myDebug >> IO.unit.myDebug
}