package part2abstractmath

import scala.util.Try

object Functors {

  val aModifiedList = List(1, 2, 3).map(_ + 1) // List(2, 3, 4)
  val aModifiedOption = Option(2).map(_ + 1) // Some(3)
  val aModifiedTry = Try(42).map(_ + 1) // Success(43)

  // Functor - generalizes the map function, higher-kinded TC

  // simplified definition
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list.given // includes Functor[List]

  val listFunctor = Functor[List]

  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1) // List(2, 3, 4)

  import cats.instances.option.given // includes Functor[Option]
  val optionFunctor = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(2))(_ + 1) // Some(3)

  import cats.instances.try_.given
  val anIncrementedTry = Functor[Try].map(Try(42))(_ + 1) // Success(43)

  // so far it didn't bring anything on top of the usual map methods defined on the data structures directly

  // generalizing an API
  def do10xList(list: List[Int]): List[Int] = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attempt: Try[Int]): Try[Int] = attempt.map(_ * 10)

  // generalize to any container by ENFORCING the presence of the map method (which we didn't have before)
  // in the presence of the implicit functor instance, this is the only method I need
  def do10x[F[_]](container: F[Int])(using functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)

  // Exercise 1: define your own functor for a binary tree
  // hint: instead of using Functor.instance, extend Functor
  trait Tree[+T]

  object Tree {
    // "smart" constructors. Could probably define two apply methods
    def leaf[T](value: T): Tree[T] = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }

  case class Leaf[+T](value: T) extends Tree[T]
  case class Branch[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T]

  given Functor[Tree] with {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value)                => Leaf(f(value))
      case Branch(value, left, right) => Branch(f(value), map(left)(f), map(right)(f))
    }
  }

  // extension method - map
  import cats.syntax.functor.given
  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1)

  // Exercise 2: write a shorter version of the do10x method by using the extension methods
  def do10xShorter[F[_]: Functor](container: F[Int]): F[Int] = container.map(_ * 10)

  /*
    Functor use cases - data structures meant to be transformed in sequence
    - specialized data structures for high-performance computing (not part of standard library)
    - any "mappable" structures under the same high-level API
   */

  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(2)))
    println(do10x(Try(35)))

    // Cats type classes are invariant, therefore I need to specify the type or have smart constructors
    println(do10x(Tree.branch(1, Tree.branch(2, Tree.leaf(3), Tree.leaf(4)), Tree.leaf(5))))

    println(do10xShorter(List(1, 2, 3)))
    println(do10xShorter(Option(2)))
    println(do10xShorter(Try(35)))
  }
}
