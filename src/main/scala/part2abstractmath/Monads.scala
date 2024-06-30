package part2abstractmath

import java.util.concurrent.Executors
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

object Monads {

  // list
  val numbersList = List(1, 2, 3)
  val charsList = List('a', 'b', 'c')

  // Exercise 1.1 - create all combinations of (number, char)

  // two below solutions are identical (compiler unwraps the for comprehension)
  val combinations = for {
    number <- numbersList
    character <- charsList
  } yield (number, character)

  val combinations2 = numbersList.flatMap(number => charsList.map(character => (number, character)))

  // option
  val numberOption = Option(2)
  val charOption = Option('d')

  // Exercise 1.2 - create a combination of (number, char)
  val combination1 = numberOption.flatMap(number => charOption.map(character => (number, character)))
  val combination2 = for {
    number <- numberOption
    character <- charOption
  } yield (number, character)

  // futures
  val executorService = Executors.newFixedThreadPool(8)
  given ExecutionContext = ExecutionContext.fromExecutorService(executorService)

  val numberFuture = Future(42)
  val charFuture = Future('Z')

  // Exercise 1.3 - create a combination of (number, char)
  val futureCombination1 = numberFuture.flatMap(number => charFuture.map(character => (number, character)))
  val futureCombination2 = for {
    number <- numberFuture
    character <- charFuture
  } yield (number, character)

  /*
    Pattern above consists of 2 fundamental abilities/operations
    - wrapping a value into a monadic value
    - transformation of values with flatMap

    FlatMap is not necessarily an iteration, more like a transformation. But it guarantees sequential evaluation order

    Monad - a higher-kinded type class with the above abilities
   */

  trait MyMonad[M[_]] {
    // Transforms a normal value of type A into a wrapped value of type M[A]
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] // very similar to Functor

    // Exercise 3: implement the map method in MyMonad
    // In fact, Monad implements the map function like this, so it's not abstract
    // Monad extends Functor and provides the map method like this
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))
  }

  // Cats Monad
  import cats.Monad
  import cats.instances.option.given // given Monad[Option]

  val optionMonad = Monad[Option]

  val anOption = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None) // None

  import cats.instances.list.given // given Monad[List]

  val listMonad = Monad[List]

  val aList = listMonad.pure(3) // List(3)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(4, 5)

  // Exercise: use a Monad[Future]
  import cats.instances.future.given
  val futureMonad = Monad[Future] // requires an implicit ExecutionContext

  val aFuture = futureMonad.pure(42)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 1)) // Future(43), eventually Success(43)

  // For now it doesn't bring anything new but it's useful for defining some general APIs

  // specialized API - a lot of repetition will happen, implementation is the same
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))

  def getPairsOption(number: Option[Int], character: Option[Char]): Option[(Int, Char)] =
    number.flatMap(n => character.map(c => (n, c)))

  def getPairsFuture(number: Future[Int], character: Future[Char]): Future[(Int, Char)] =
    number.flatMap(n => character.map(c => (n, c)))

  // general API - need one method only
  def getPairs[M[_], A, B](ma: M[A], mb: M[B])(using monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods (weirder imports) - pure, flatMap
  import cats.syntax.applicative.given // pure (applicatives are weaker monads)
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be used
  val oneList = 1.pure[List] // List(1)

  import cats.syntax.flatMap.given // flatMap method imported
  val oneOptionTransformed = oneOption.flatMap(x => (x + 1).pure[Option])

  // Since Monad extends Functor, I can use the map method, either directly or as extension method
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1)
  import cats.syntax.functor.given // map method imported
  val oneOptionMapped2 = oneOption.map(_ + 2)

  // Since map and flatMap are available, we can use for-comprehensions
  val composedOptionFor = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two

  // Exercise 4: implement a shorter version of getPairs using for-comprehension
  def getPairsShort[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] =
    for {
      a <- ma
      b <- mb
    } yield (a, b)

  /*
    Monad use cases - sequential transformations
    - list combinations
    - option transformations
    - asynchronous chained computations
    - dependent computations
   */
  // For-comprehensions are NOT iteration, rather FlatMap is a mental model of chained transformations.

  def main(args: Array[String]): Unit = {
    println(combinations)
    println(combinations2)
    println(combination1)
    println(combination2)
    println(Await.result(futureCombination1, Duration.Inf))
    println(Await.result(futureCombination2, Duration.Inf))

    println(getPairs(numbersList, charsList))
    println(getPairs(numberOption, charOption))
    getPairs(numberFuture, charFuture).foreach(println)

    println(getPairsShort(numbersList, charsList))
    println(getPairsShort(numberOption, charOption))
    getPairsShort(numberFuture, charFuture).foreach(println)

    executorService.shutdown()
  }
}
