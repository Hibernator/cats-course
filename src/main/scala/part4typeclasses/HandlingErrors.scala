package part4typeclasses

import cats.{Applicative, Monad}

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object HandlingErrors:

  // Three levels of error handling maturity
  // 1. Try/catch
  // 2. Using Scala Try
  // 3. pure FP with Cats

  /** @tparam M
    *   wrapper over a value
    * @tparam E
    *   error type, doesn't have to be an actual JVM exception, can be anything
    */
  trait MyApplicativeError[M[_], E] extends Applicative[M]:
    // fundamental method
    // E doesn't have to be an exception, can be any type. A very general error representation

    // Since MonadError extends Monad, all Monad methods are available
    /** @param e
      *   error to raise
      * @tparam A
      *   value type
      * @return
      *   a wrapper over the value that incorporates the error inside if there is one
      */
    def raiseError[A](e: E): M[A]

    // also fundamental method
    def handleErrorWith[A](ma: M[A])(func: E => M[A]): M[A]

    def handleError[A](ma: M[A])(func: E => A): M[A] = handleErrorWith(ma)(e => pure(func(e)))

  /** @tparam M
    *   wrapper over a value
    * @tparam E
    *   error type, doesn't have to be an actual JVM exception, can be anything
    */
  trait MyMonadError[M[_], E] extends MyApplicativeError[M, E] with Monad[M]:
    // fundamental method of MonadError. The other methods are inherited from ApplicativeError
    def ensure[A](ma: M[A])(error: E)(predicate: A => Boolean): M[A]

  import cats.MonadError
  // instances can be created by compiler if there is a Monad in scope
  // most MonadErrors are Monads with additional raiseError method
  import cats.instances.either.given // given MonadError with Either as error type
  type ErrorOr[A] = Either[String, A]
  // It's important the error type of MonadError matches error type of Either
  val monadErrorEither: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]
  val success: ErrorOr[Int] = monadErrorEither.pure(32) // Either[String, Int], Right(32)
  val failure: ErrorOr[Int] =
    monadErrorEither.raiseError[Int]("something wrong") // Either[String, Int], Left("something wrong")

  // there are other convenience methods available
  // recovering from an error: equivalents of recover and recoverWith of Try

  // "recover"
  val handledError: ErrorOr[Int] = monadErrorEither.handleError(failure) {
    case "Badness" => 44
    case _         => 89
  }

  // "recoverWith"
  val handledError2: ErrorOr[Int] = monadErrorEither.handleErrorWith(failure) {
    case "Badness" => monadErrorEither.pure(44) // ErrorOr[Int]
    case _         => Left("Something else") // ErrorOr[Int]
  }

  // "filter"
  val filteredSuccess: ErrorOr[Int] = monadErrorEither.ensure(success)("Number too small")(_ > 100)

  // MonadError instances for Try and Future
  import cats.instances.try_.given // given MonadError[Try], E = Throwable
  val exception = new RuntimeException("Really bad")

  // We're not throwing the exception here but rather storing it
  val pureException: Try[Int] = MonadError[Try, Throwable].raiseError(exception) // Try[Int], Failure(exception)

  import cats.instances.future.given
  given ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  MonadError[Future, Throwable].raiseError(exception) // Future which will complete with a Failure(exception)

  // above we have 3 completely different implementations of the raiseError API, applied to different monadic types

  // For applicatives (weaker monads), there exists ApplicativeError
  import cats.data.Validated
  // Semigroup[List] to enable combine operation on the error type.
  // Compiler will then be able to create ApplicativeError[ErrorsOr, List[String]]
  import cats.instances.list.given
  type ErrorsOr[T] = Validated[List[String], T]
  import cats.ApplicativeError
  val applErrorVal: ApplicativeError[ErrorsOr, List[String]] = ApplicativeError[ErrorsOr, List[String]]
  // ApplicativeError extends Applicative, the API is the same as of MonadError
  // pure, raiseError, handleError, handleErrorWith
  // in fact raiseError is not a fundamental method of MonadError but of ApplicativeError

  // extension methods
  import cats.syntax.applicative.given // pure extension method
  import cats.syntax.applicativeError.given // raiseError, handleError, handleErrorWith extension method

  // same as applErrorVal.pure(42)
  val extendedSuccess: ErrorsOr[Int] = 42.pure[ErrorsOr] // requires given ApplicativeError[ErrorsOr, List[String]]
  // same as applErrorVal.raiseError(List("Badness"))
  val extendedError: ErrorsOr[Int] = List("Badness").raiseError[ErrorsOr, Int]
  // same as applErrorVal.handleError
  val recoveredError: ErrorsOr[Int] = extendedError.recover { case _ => 43 }

  import cats.syntax.monadError.given // ensure extension method
  val testedSuccess: ErrorOr[Int] = success.ensure("Something bad")(_ > 100)

  def main(args: Array[String]): Unit = {}
