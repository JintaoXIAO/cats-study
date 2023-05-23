package jt

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.kernel.Ref
import scala.concurrent.duration._
import cats.syntax.parallel._
import utils._

object Refs extends IOApp.Simple {
  
  val atomicMol: IO[Ref[IO, Int]] = Ref[IO].of(42)
  val atomicMol_v2: IO[Ref[IO, Int]] = IO.ref(42)

  val increasedMol: IO[Unit] = atomicMol.flatMap {_.set(543)}
  val mol: IO[Int] = atomicMol.flatMap {_.get}
  val gsMol: IO[Int] = atomicMol.flatMap { _.getAndSet(43) }
  val fMol: IO[Unit] = atomicMol.flatMap { _.update( _ + 1 ) } 
  val updatedMol: IO[Int] = atomicMol.flatMap { _.updateAndGet( _ + 1 ) }
  val modifiedMol: IO[String] = atomicMol.flatMap { _.modify(value => (value * 10, s"my current value is $value")) }

  def demoConcurrentWorkImpure(): IO[Unit] = {
    var count = 0

    def task(workload: String): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for `$workload`: $wordCount").myDebug
        newCount <- IO(count + wordCount)
        _ <- IO(s"New total: $newCount").myDebug
        _ <- IO(count += newCount)
      } yield () 
    }
    List("I love Cats Effect", "This ref thing is useless", "hello world")
      .map(task) 
      .parSequence
      .void
  }

  def demoConcurrentWorkPure() = {

    def task(workload: String, total: Ref[IO, Int]): IO[Unit] = {
      val wordCount = workload.split(" ").length
      for {
        _ <- IO(s"Counting words for `$workload`: $wordCount").myDebug
        newCount <- total.updateAndGet(_ + wordCount)
        _ <- IO(s"New total: $newCount").myDebug
      } yield () 
    }

    for { 
      initialCount <- Ref[IO].of(0)
      _ <- List("I love Cats Effect", "This ref thing is useless", "hello world")
              .map(task(_,initialCount)) 
              .parSequence
    } yield ()     
  }  

  def tickingClockImpure(): IO[Unit] = {
    var ticks = 0L
    def tickingClock: IO[Unit] = for {
      _ <- IO.sleep(1.second) 
      _ <- IO(System.currentTimeMillis).myDebug
      _ <- IO(ticks += 1)
      _ <- tickingClock
    } yield ()

    def printTicks: IO[Unit] = for {
      _ <- IO.sleep(5.seconds)  
      _ <- IO(s"TICKS: $ticks").myDebug
      _ <- printTicks
    } yield ()

    for {
      _ <- (tickingClock, printTicks).parTupled  
    } yield ()        
  }

  def tickingClockPure(): IO[Unit] = {
    var ticks = IO.ref(0L)

    def tickingClock(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(1.second) 
      _ <- IO(System.currentTimeMillis).myDebug
      _ <- ticks.update(_ + 1L)
      _ <- tickingClock(ticks)
    } yield ()

    def printTicks(ticks: Ref[IO, Long]): IO[Unit] = for {
      _ <- IO.sleep(5.seconds)  
      tick <- ticks.get
      _ <- IO(s"TICKS: $tick").myDebug
      _ <- printTicks(ticks)
    } yield ()

    for {
      tick <- ticks
      _ <- (tickingClock(tick), printTicks(tick)).parTupled  
    } yield ()        
  }

  override def run: IO[Unit] = 
    // demoConcurrentWorkImpure()
    // demoConcurrentWorkPure()
    // tickingClockImpure()
    tickingClockPure()
}