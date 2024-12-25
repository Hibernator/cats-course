package part4typeclasses

object Applicatives:

  // Applicative is an extension of Functor that introduces a pure method (wraps a value)
  // Applicative = Functor + pure method

  import cats.Applicative
  import cats.instances.list.given

  val listApplicative: Applicative[List] = Applicative[List]
  val aList: List[Int] = listApplicative.pure(2) // List(2)

  import cats.instances.option.given // given Applicative[Option]
  val optionApplicative: Applicative[Option] = Applicative[Option]
  val anOption: Option[Int] = optionApplicative.pure(2) // Some(2)

  // Applicative is a functor on steroids. The map function helps generalizing an API, plus there is pure function

  // pure extension method
  import cats.syntax.applicative.given
  val aSweetList: List[Int] = 2.pure[List] // List(2)
  val aSweetOption: Option[Int] = 2.pure[Option] // Some(2)

  // The Monad.pure method is actually inherited from Applicative. It's the same method
  // Monad extends Applicative
  // Applicative extends Functor
  // Applicatives are rarely used independently, because most of them are Monadic structures
  // Exception: Validated. It doesn't respect monadic laws for combining but can be mapped and values wrapped in it

  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val aValidValue: ErrorsOr[Int] = Validated.valid(43) // like a pure method
  val aModifiedValidated: ErrorsOr[Int] = aValidValue.map(_ + 1) // map
  val validatedApplicative: Applicative[ErrorsOr] = Applicative[ErrorsOr]

  // Exercise - thought experiment. Implement the productWithApplicatives function
  // Use the fundamental Applicative methods: map, pure.
  // In fact, it's impossible
  // But it's possible with the help of the following helper function (assume it's already implemented)
  def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B] = ??? // this is already a method on Applicative

  def productWithApplicatives[W[_], A, B](wa: W[A], wb: W[B])(using applicative: Applicative[W]): W[(A, B)] =
    val functionWrapper: W[B => (A, B)] = applicative.map(wa)(a => (b: B) => (a, b))
    applicative.ap(functionWrapper)(wb)

  // Applicative has the ap method: ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B]
  // Applicative can implement the Semigroupal product method because of the presence of the helper ap method
  // Applicative extends Semigroupal

  val productOfList: List[(Int, String)] = productWithApplicatives(
    List(1, 2),
    List("a", "b")
  )

  def main(args: Array[String]): Unit = {
    println(productOfList)
  }
