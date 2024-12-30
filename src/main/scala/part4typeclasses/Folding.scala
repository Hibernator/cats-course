package part4typeclasses

import cats.Eval
import cats.kernel.Monoid

object Folding:

  // Exercise - implement methods on List in terms of the foldLeft and foldRight method
  object ListExercises:
    // using foldRight and :: operator would be more efficient
    def map[A, B](list: List[A])(f: A => B): List[B] = list.foldLeft(List())(_ :+ f(_))
    def mapDaniel[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((value, acc) => f(value) :: acc)
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] = list.foldLeft(List())(_ ++ f(_))
    def flatMapDaniel[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((acc, value) => acc.foldRight(f(value))(_ :: _))
    def filter[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldLeft(List())((acc, value) => if predicate(value) then acc :+ value else acc)
    def filterDaniel[A](list: List[A])(predicate: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((value, acc) => if predicate(value) then value :: acc else acc)
    def combineAll[A](list: List[A])(using monoid: Monoid[A]): A = list.foldLeft(monoid.empty)(monoid.combine)

  import cats.Foldable
  import cats.instances.list.given // Foldable[List]
  // fundamental methods are foldLeft and foldRight
  val sum: Int = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  // the Foldable API works as expected for many other data structures
  import cats.instances.option.given // given Foldable[Option]
  val sumOption: Int = Foldable[Option].foldLeft(Option(30), 2)(_ + _) // 32

  // Foldable is very useful for generalized APIs, which will work for any data structure,
  // for which an implicit Foldable is present

  // foldRight method
  // foldRight is stack-safe because it uses Eval, which is stack-safe regardless of container implementation
  // foldRight makes any container stack-safe
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }

  // Foldable can provide extra functionality if Monoid is present
  import cats.instances.int.given // Monoid[Int]
  val anotherSum: Int = Foldable[List].combineAll(List(1, 2, 3))
  import cats.instances.string.given // Monoid[String]
  // first maps and then combines
  val mappedConcat: String = Foldable[List].foldMap(List(1, 2, 3))(_.toString)

  // deep traversals for nested foldable structures
  val intsNested: List[Vector[Int]] = List(Vector(1, 2, 3), Vector(4, 5, 6))
  import cats.instances.vector.given // Foldable[Vector]
  // two statements below are the same
  Foldable[List].compose(using Foldable[Vector]).combineAll(intsNested)
  Foldable[List].compose[Vector].combineAll(intsNested)

  // extension methods
  import cats.syntax.foldable.given
  val sum3: Int = List(1, 2, 3).combineAll // requires Foldable[List], Monoid[Int]
  val mappedConcat2: String = List(1, 2, 3).foldMap(_.toString) // requires Foldable[List], Monoid[String]

  def main(args: Array[String]): Unit = {
    val numbers = List(1, 2, 3, 4)
    println(ListExercises.map(numbers)(_ + 1))
    println(ListExercises.flatMap(numbers)(value => List(value, value + 1)))
    println(ListExercises.filter(numbers)(_ % 2 == 0))
    println(ListExercises.combineAll(numbers))
  }
