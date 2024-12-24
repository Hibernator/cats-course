package part4typeclasses

import cats.Monad

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object Semigroupals:

  trait MySemigroupal[F[_]]:
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  import cats.Semigroupal
  import cats.instances.option.given // given Semigroupal[Option]

  val optionSemigroupal: Semigroupal[Option] = Semigroupal[Option]

  // Some((123, "a string"))
  val aTupledOption: Option[(Int, String)] = optionSemigroupal.product(Option(123), Option("a string"))
  val aNoneTupled: Option[(Int, Nothing)] = optionSemigroupal.product(Option(123), None)

  import cats.instances.future.given // Semigroupal[Future]
  val threadPool = Executors.newFixedThreadPool(8)
  given ExecutionContext = ExecutionContext.fromExecutorService(threadPool)

  // The futures will run in parallel and once they are finished, they will be combined into a Future of tuple
  // Future(("a meaning of life", 42))
  val aTupledFuture: Future[(String, Int)] = Semigroupal[Future].product(Future("a meaning of life"), Future(42))

  import cats.instances.list.given
  val aTupledList: List[(Int, String)] = Semigroupal[List].product(List(1, 2), List("a", "b"))

  // Exercise - product with monads
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(using monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  import cats.syntax.functor.given // for map
  import cats.syntax.flatMap.given // for flatMap
  def productWithMonadsFor[F[_], A, B](fa: F[A], fb: F[B])(using monad: Monad[F]): F[(A, B)] =
    for
      a <- fa
      b <- fb
    yield (a, b)

  // Monad extends Semigroupal - common pattern in cats: define more complex structures in terms of simpler ones

  // Why are Semigroupals useful if we could just use Monads?
  // Because Monads follow rules, that impose sequencing of computations (for comprehension)
  // But we might want to do a product (combination) differently, not following the Monad laws
  // Validated is an example of that

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]

  // requires given Semigroup[List], provided above
  val validatedSemigroupal: Semigroupal[ErrorsOr] = Semigroupal[ErrorsOr]
  // now we can combine instances of Validated without following Monad laws, because they wouldn't make sense here
  // We don't really want a cartesian product here, we just want to combine the lists
  val invalidsCombination: ErrorsOr[(Nothing, Nothing)] = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either.given // given Monad[Either]
  val eitherSemigroupal: Semigroupal[EitherErrorsOr] = Semigroupal[EitherErrorsOr]

  // the second either will be omitted, because flatMap on Either short-circuits it (right-biased)
  // So for tracking errors, Validated is more useful than Either
  val eitherCombination: EitherErrorsOr[(Nothing, Nothing)] = eitherSemigroupal.product( // in terms of map/flatMap
    Left(List("Something wrong", "Something else wrong")),
    Left(List("This can't be right"))
  )
  // the associativity law is now useful here: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  // Exercise 2: define Semigroupal[List] that does zip, not cartesian product
  val zipSemigroupal: Semigroupal[List] = new Semigroupal[List]:
    override def product[A, B](fa: List[A], fb: List[B]): List[(A, B)] = fa zip fb

  val zippedList: Seq[(Int, String)] = zipSemigroupal.product(List(1, 2), List("a", "b"))

  /*
    Summary
    - higher-kinded type that can tuple elements wrapped in another data structure - product method
    - Monads extend Semigroupal and implement product in terms of map/flatMap
    - some Semigroupals are useful without being a Monad - Validated
    - Semigroup vs Semigroupal - combine vs tuple
   */

  def main(args: Array[String]): Unit = {
    println(aTupledList)
    println(productWithMonads(List(1, 2), List("a", "b")))
    println(productWithMonadsFor(List(1, 2), List("a", "b")))
    println(invalidsCombination)
    println(eitherCombination)
    println(zippedList)

    threadPool.shutdown()
  }
