package jt



object MonadTransformers extends App {

  case class User(name: String)  

  def lookupUser(id: Long): Either[Error, Option[User]] = ???

  def lookupUserName(id: Long): Either[Error, Option[String]] = 
    for {
      optUser <- lookupUser(id)  
    } yield {
      for { user <- optUser } yield user.name
    } 
}

object Example01 extends App {
  import cats.data.OptionT
  import cats.syntax.option._
  import cats.syntax.applicative._

  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(10.some, 20.some, 30.some))
  val result2: ListOption[Int] = 32.pure[ListOption]

  val r: ListOption[Int] = result1.flatMap{ x => 
    result2.map{ y => 
        x + y  
      }  
    }

  println(r)    
  println(r.isEmpty)
}

object Example02 extends App {
  import cats.data.OptionT  
  import cats.data.EitherT
  import cats.syntax.applicative._
  import cats.syntax.apply._

  type ErrorOr[A] =Either[String, A]
  type ErrorOrOption[A] = OptionT[ErrorOr, A]

  val a = 10.pure[ErrorOrOption]
  val b = 32.pure[ErrorOrOption]
  val c: ErrorOrOption[(Int, Int)] = (a,b).mapN{(_,_)}

  println(c)  

  val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
  val errorStack2 = 32.pure[ErrorOrOption]

  println(errorStack1)  
  println(errorStack1.value)
  println(errorStack1.value.isRight)

}

object Example03 extends App {
  import scala.concurrent.Future
  import cats.syntax.applicative._
  import cats.data.{ EitherT, OptionT }
  import cats.instances.future._
  import scala.concurrent.Await
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  type FutureEither[A] = EitherT[Future, String, A]
  type FutureEitherOption[A] = OptionT[FutureEither, A]

  val futureEitherOp: FutureEitherOption[Int] = 
    for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b 

  val intermediate: FutureEither[Option[Int]] = futureEitherOp.value
  val stack: Future[Either[String, Option[Int]]] = intermediate.value
  Await.result(stack, 1.second)    
}

object Example04 extends App {
  import cats.data.Writer

  type Logged[A] = Writer[List[String], A]  

  def parseNumber(str: String): Logged[Option[Int]] =
    util.Try(str.toInt).toOption match {
      case Some(n) => Writer(List(s"Read $str"), Some(n))
      case None => Writer(List(s"Failed on $str"), None)
    }

  def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
    import cats.data.OptionT

    val r = for {
      a <- OptionT(parseNumber(a))
      b <- OptionT(parseNumber(b))
      c <- OptionT(parseNumber(c))
    } yield a + b + c    
    r.value
  }

  println(addAll("1", "12", "23").run)
}

object Exercise extends App {
  import scala.concurrent.ExecutionContext.Implicits.global  
  import scala.concurrent.{ Future, Await }
  import cats.data.EitherT
  import scala.concurrent.duration._
  import cats.syntax.applicative._
  import cats.syntax.either._
  
  // type Response[A] = Future[Either[String, A]]
  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "jazz" -> 6,
    "bumblebee" -> 8,
    "hot rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = 
    powerLevels.get(autobot) match {
      case None => EitherT((s"$autobot is unreachable".asLeft[Int]).pure[Future])
      case Some(value) => EitherT((value.asRight[String]).pure[Future])
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = 
    for {
      a <- getPowerLevel(ally1)
      b <- getPowerLevel(ally2)
    } yield a + b > 15    

  def tacticalReport(ally1: String, ally2: String): String = {
    val m: Response[Boolean] = canSpecialMove(ally1, ally2)      
    val f: Future[Either[String, Boolean]] = m.value
    Await.result(f, 1.second) match {
      case Right(b) => 
        if (b) {
          s"$ally1 and $ally2 are ready to roll out!"
        } else {
          s"$ally1 and $ally2 need a recharge."
        } 
      case Left(err) => s"Comms error: $err"
    }
  }
  
  println(tacticalReport("jazz", "ironhide"))
  println(tacticalReport("bumblebee", "hot rod"))
  println(tacticalReport("jazz", "bumblebee"))
}