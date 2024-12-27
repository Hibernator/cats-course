package part4typeclasses

import cats.{Applicative, Apply, FlatMap}

object WeakerMonads:

  // Question: does FlatMap extend Apply? (would be implied from the class hierarchy)
  // If so, it should implement the ap method
  trait MyFlatMap[M[_]] extends Apply[M]:
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // Exercise: implement this method
    // hint: Apply extends Functor (map method available). Should be possible with the help of map and flatMap
    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] =
      flatMap(wa)(a => map(wf)(f => f(a)))
    //         |  |        /   \     \/
    //         |  |    M[A=>B] A=>B  B
    //         |  |    \_____   ____/
    //       M[A] A =>      M[B]

  // In cats, Monad extends exactly those traits
  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M]:
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(x => pure(f(x)))

  // FlatMap is rarely used independently, because there are stronger flatMaps available: Monads
  // But their extension methods are useful and have to be imported even if Monads are used
  import cats.syntax.flatMap.given // flatMap extension method
  import cats.syntax.functor.given // map extension method
  // Now for-comprehensions are possible if we have a Monad present

  // Very general method
  def getPairs[M[_]: FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] =
    for
      n <- numbers
      c <- chars
    yield (n, c)

  // Summary
  // FlatMap (Weaker Monad) = Apply + flatMap function
  // Monad actually doesn't have any new fundamental methods of its own
  // It extends FlatMap (flatMap method) and Applicative (pure method)
  // FlatMap also extends Apply which extends Functor, so the map method is also available: for-comprehensions possible

  def main(args: Array[String]): Unit = {}
