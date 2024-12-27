package part4typeclasses

import cats.{Functor, Semigroupal}

object WeakerApplicatives:

  // Fundemental method of Apply is ap
  // Apply also has a product method, which is implemented exactly as we have here (and did in the previous lesson)
  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W]:
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] =
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)

    // Exercise - implement mapN in the presence of ap and product
    // In fact, I didn't need the ap method
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] =
      val tupleWrapper = product(tuple._1, tuple._2)
      map(tupleWrapper) { case (a, b) =>
        f(a, b)
      }

    // Takes a wrapper over function and a wrapper over value
    // returns a wrapper over another value obtained by function application
    def ap[B, T](wf: W[B => T])(wa: W[B]): W[T] = ??? // fundamental method of Apply

  // Instead of extending both Functor and Semigroupal, Applicative extends just Apply
  trait MyApplicative[W[_]] extends MyApply[W]:
    def pure[A](x: A): W[A] // fundemental Applicative method

  import cats.Apply
  import cats.instances.option.given // given Apply[Option]
  val applyOption: Apply[Option] = Apply[Option]
  val funcApp: Option[Int] = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)

  // ap method is rarely used independently by itself
  // but it allows us to call product method with up to 22 elements
  // and we can apply functions with up to 22 elements in the ap method

  import cats.syntax.apply.given // extension methods of Apply
  val tupleOfOptions: (Option[Int], Option[Int], Option[Int]) = (Option(1), Option(2), Option(3))
  // Can be transformed into Option of tuple thanks to the product and ap methods
  val optionOfTuple: Option[(Int, Int, Int)] = tupleOfOptions.tupled // Some(1, 2, 3)
  // I can also apply a function on all those values without the need to unwrap them
  // Works up to 22 arguments
  val sumOption: Option[Int] = tupleOfOptions.mapN(_ + _ + _) // Some(6)

  // Summary
  // Apply = Functor + Semigroupal + ap
  // ap method:
  // Takes a wrapper over function and a wrapper over value
  // returns a wrapper over another value obtained by function application
  // Useful extension methods: tupled, mapN

  def main(args: Array[String]): Unit = {}
