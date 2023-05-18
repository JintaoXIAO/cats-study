package jt

package semigroupal
object Example01 extends App {
  import cats.syntax.either._

  def parseInt(str: String): Either[String, Int] = 
    Either.catchOnly[NumberFormatException](str.toInt)
      .leftMap(_ => s"Couldn't read $str")   

  val a = 
    for {
      a <- parseInt("a")  
      b <- parseInt("b")
      c <- parseInt("c")
    } yield a + b + c      
  println(a)
}

object Example02 extends App {
  import cats.Semigroupal
  import cats.syntax.option._
  import cats.syntax.apply._
  import cats.instances.option._

  val a = Semigroupal[Option].product(Some(123), Some("abc"))    
  println(a)
  val b = Semigroupal[Option].product(Some(1), None)
  println(b)
  val c = Semigroupal.tuple3(1.some, 2.some, 3.some)
  println(c)
  val d = Semigroupal.map3(1.some, 2.some, 3.some)(_ + _ + _)
  println(d)

  val e = (123.some, "abc".some, true.some).tupled
  println(e)

}

object Example03 extends App {
  import cats.Monoid
  import cats.instances.int._
  import cats.instances.invariant._
  import cats.instances.list._
  import cats.instances.string._
  import cats.syntax.apply._
  import cats.syntax.semigroup._


  final case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

  val tupleToCat: (String, Int, List[String]) => Cat = Cat.apply _ 
  val catToTuple: Cat => (String, Int, List[String]) = 
    cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)  

  implicit val catMonoid: Monoid[Cat] = (Monoid[String], Monoid[Int], Monoid[List[String]]).imapN(tupleToCat)(catToTuple)    

  val garfield = Cat("Garfield", 1923, List("Lasagne"))
  val heathcliff = Cat("Heathcliff", 1922, List("Junk Food"))

  val a = garfield |+| heathcliff  
  println(a)

}

object Example04 extends App {
  import cats.Semigroupal
  import cats.instances.future._
  import cats.syntax.apply._
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global


  var futurePair = Semigroupal[Future].product(Future("hello"), Future(123))
  val a = Await.result(futurePair, 1.second)
  println(a)

  case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])  
  val futureCat = (Future("Garfield"), Future(1234), Future(List("Lasagne"))).mapN(Cat.apply)

  val c = Await.result(futureCat, 1.second)  
  println(c)

}

object Example05 extends App {
  import cats.Semigroupal
  import cats.instances.list._
  import cats.instances.either._
  import cats.syntax.either._

  val a = Semigroupal[List].product(List(1,2), List("a", "b", "c"))  
  println(a)

  type ErrorOr[A] = Either[Vector[String], A]  

  val b: Either[Vector[String], Any] = Semigroupal[ErrorOr].product(Left(Vector("Error01")), Left(Vector("Error2")))
  println(b)
}