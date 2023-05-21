package jt

import cats.effect.IOApp
import cats.effect.IO
import scala.concurrent.duration._
import utils._
import java.util.Scanner
import java.io.FileReader
import java.io.File

object Resources extends IOApp.Simple {

  class Connection(url: String) {
    def open: IO[String] = IO(s"opening connection to $url").myDebug
    def close: IO[String] = IO(s"closing connection to $url").myDebug  
  } 

  val asyncFetchUrl = for {
    fib <- new Connection("rocketjvm.com").open *> IO.sleep(Int.MaxValue.seconds).start  
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()     

  val correctAsyncFetchUrl = for {
    conn <- IO(new Connection("rocketjvm.com"))  
    fib <- conn.open *> IO.sleep(Int.MaxValue.second).onCancel(conn.close.void).start 
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()     

  val bracketFetchUrl: IO[String] = IO(new Connection("rocketjvm.com"))
    .bracket(conn => conn.open <* IO.sleep(Int.MaxValue.seconds))(conn => conn.close.void)

  val bracketProgram = for {
    fib <- bracketFetchUrl.start
    _ <- IO.sleep(1.second) *> fib.cancel
  } yield ()
  
  def openFileScanner(path: String): IO[Scanner] = 
    IO(new Scanner(new FileReader(new File(path))))  

  def readLineByLine(scanner: Scanner): IO[Unit] = 
    IO.whenA(scanner.hasNextLine()) { 
        IO(scanner.nextLine()).myDebug *> IO.sleep(100.millis) >> readLineByLine(scanner)
    }

  def bracketReadFile(path: String): IO[Unit] = 
    openFileScanner(path).bracket{ scanner => readLineByLine(scanner) } { scanner => IO{ scanner.close() }} 

  override def run: IO[Unit] = 
    // asyncFetchUrl.void    
    // correctAsyncFetchUrl.void
    // bracketProgram
    bracketReadFile("./.gitignore")
}