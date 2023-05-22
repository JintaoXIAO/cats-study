package jt

import scala.concurrent.duration._
import cats.effect.IOApp
import utils._
import cats.effect.IO
import cats.instances.seq

object CancellingIOs extends IOApp.Simple {
  
  val chainOfIOs: IO[Int] = IO("waiting").myDebug >> IO.canceled >> IO(42).myDebug

  val specialPaymentSystem =
    (    
      IO("Payment running, don't cancel me...").myDebug >>
      IO.sleep(1.second) >>
      IO("Payment compelted.").myDebug
    ).onCancel(IO("MEGA CANCEL OF DOOM!").myDebug.void)

  val cancellationOfDoom = for {
    fib <- specialPaymentSystem.start
    _ <- IO.sleep(500.milliseconds) >> fib.cancel
    _ <- fib.join
  } yield ()

  val atomicPayment = IO.uncancelable(_ => specialPaymentSystem)
  val atomicPayment_v2 = specialPaymentSystem.uncancelable 

  val noCancellationOfDoom = for {
    fib <- atomicPayment.start
    _ <- IO.sleep(500.milliseconds) >> IO("attempting cancellation...").myDebug >> fib.cancel
    _ <- fib.join
  } yield ()

  val inputPassword = IO("Input password").myDebug >> IO("typing password").myDebug >> IO.sleep(2.seconds) >> IO("Rockthejvm")
  val verifyPassword = (pw: String) => IO("verifying...").myDebug >> IO.sleep(2.second) >> IO(pw == "Rockthejvm")

  val authFlow: IO[Unit] = IO.uncancelable { poll => 
    for {
      pw <- poll(inputPassword).onCancel(IO("Authentication timeout. Try again later.").myDebug.void)  
      verified <- verifyPassword(pw)
      _ <- if (verified) IO("Authentication successful.").myDebug
           else IO("Authentication failed").myDebug
    } yield () 
  }  

  val authProgram = for {
    authFib <- authFlow.start
    _ <- IO.sleep(1.seconds) >> IO("Authentication timeout, attempting cancel...").myDebug >> authFib.cancel
    _ <- authFib.join  
  } yield ()

  val cancelBeforeMol = IO.canceled >> IO(42).myDebug
  val unCancelableMol = IO.uncancelable(_ => IO.canceled >> IO(42).myDebug)

  val invincibleAuthProgram = for {
    authFib <- IO.uncancelable(_ => authFlow).start
    _ <- IO.sleep(1.seconds) >> IO("Authentication timeout, attempting cancel...").myDebug >> authFib.cancel
    _ <- authFib.join  
  } yield ()

  def threeStepProgram(): IO[Unit] = {
    val sequence = IO.uncancelable { poll =>
      poll(IO("cancelable").myDebug >> IO.sleep(1.second)) >>
      IO("uncancelable").myDebug >> IO.sleep(1.second) >> 
      poll(IO("<second cancelable").myDebug >> IO.sleep(1.second) >> IO("second cancelable>").myDebug) 
    }  

    for {
      fib <- sequence.start
      _ <- IO.sleep(3500.millis) >> IO("CANCELING").myDebug >> fib.cancel
      _ <- fib.join  
    } yield ()    
  }

   override def run: IO[Unit] = 
    // noCancellationOfDoom 
    // authFlow
    // autoProgram
    // cancelBeforeMol.void
    // unCancelableMol.void
    // invincibleAuthProgram
    threeStepProgram()

}