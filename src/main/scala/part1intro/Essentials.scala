package part1intro

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object Essentials {

  // values
  val aBoolean: Boolean = false

  // expressions are EVALUATED to a value
  val anIfExpression: String = if 2 > 3 then "bigger" else "smaller"

  // instructions vs expression
  val theUnit: Unit = println("Hello, Scala") // Unit = "void" in other languages. Only one value ()

  // OOP
  class Animal
  class Cat extends Animal
  trait Carnivore {
    def eat(animal: Animal): Unit
  }

  // inheritance model: extend <= 1 class, but inherit from >= 0 traits
  class Crocodile extends Animal, Carnivore:
    override def eat(animal: Animal): Unit = println("Crunch!")

  // singleton
  object MySingleton // singleton pattern in one line

  // companions
  object Carnivore // companion object of the class Carnivore

  // generics
  class MyList[A]

  // method notation
  val three: Int = 1 + 2 // infixed + method
  val anotherThree: Int = 1.+(2)

  // also prefix and postfix method notation possible

  // functional programming
  val incrementer: Int => Int = x => x + 1
  val incremented: Int = incrementer(45) // 46

  // higher-order functions. Map, flatMap, filter
  val processedList: List[Int] = List(1, 2, 3).map(incrementer) // List(2, 3, 4)
  val aLongerList: List[Int] = List(1, 2, 3).flatMap(x => List(x, x + 1)) // List(1, 2, 2, 3, 3, 4)

  // for-comprehension
  val checkerboard: List[(Int, Char)] = List(1, 2, 3).flatMap(n => List('a', 'b', 'c').map(c => (n, c)))
  val anotherCheckerboard: List[(Int, Char)] = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c)

  // options, try
  val anOption: Option[Int] = Option( /*something that might be null*/ 3) // Some(3)
  val doubledOption: Option[Int] = anOption.map(_ * 2)

  val anAttempt: Try[Int] = Try( /* something that might throw */ 42) // Success(42)
  val aModifiedAttempt: Try[Int] = anAttempt.map(_ + 10)

  // pattern matching
  val anUnknown: Any = 45
  val ordinal: String = anUnknown match
    case 1 => "first"
    case 2 => "second"
    case _ => "unknown"

  val optionDescription: String = anOption match
    case Some(value) => s"the option is not empty: $value"
    case None        => "the option is empty"

  // Futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aFuture: Future[Int] = Future {
    // a bit of code
    42
  }

  // wait for completion (async)
  aFuture.onComplete {
    case Success(value)     => println(s"The async meaning of life is $value")
    case Failure(exception) => println(s"Meaning of value failed: $exception")
  }

  // map a Future
  val anotherFuture: Future[Int] = aFuture.map(_ + 1) // Future(43) when it completes

  // partial functions
  val aPartialFunction: PartialFunction[Int, Int] = {
    case 1   => 43
    case 8   => 56
    case 100 => 999
  }

  // some more advanced stuff
  // higher-kinded types. Generic class whose type argument has its own type argument
  trait HigherKindedType[F[_]]
  trait SequenceChecker[F[_]] {
    def isSequential: Boolean
  }

  val listChecker: SequenceChecker[List] = new SequenceChecker[List]:
    override def isSequential: Boolean = true

  def main(args: Array[String]): Unit = {}
}
