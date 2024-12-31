package part5alien

import cats.kernel.Monoid

import scala.language.implicitConversions

object ContravariantFunctors:

  // Nothing to do with the usual generics co(ntra)variance in Scala

  // Example
  // Basic type class
  trait Format[T]: // contravariant type class
    self =>

    def format(value: T): String

    // In fact the contramap method can belong to Format, so let's add it here

    /** Derives additional instances of type class for another type given the transformation function from that type to
      * my type that is currently supported.
      * @param func
      * @tparam A
      * @return
      */
    def contramap[A](func: A => T): Format[A] = new Format[A]:
      override def format(value: A): String = self.format(func(value))

  // general API for format
  def format[A](value: A)(using f: Format[A]): String = f.format(value)

  given Format[String] with
    override def format(value: String): String = "\"" + value + "\""

  given Format[Int] with
    override def format(value: Int): String = value.toString

  given Format[Boolean] with
    override def format(value: Boolean): String = if value then "Y" else "N"

  // Problem: given Format[MyType], can we also have Format[Option[MyType]], Format[List[MyType]]...?
  // Can we automatically create Format of a wrapper of a type we already support?

  // This is a possible way in Scala 2
//  implicit def getOptionFormat[T](using f: Format[T]): Format[Option[T]] = new Format[Option[T]]:
//    override def format(value: Option[T]): String = f.format(value.get)

  // The same in Scala 3
  // Given the presence of given Format[T], compiler can automatically create Format[Option[T]]
//  given getOptionFormat[T](using f: Format[T]): Conversion[Format[T], Format[Option[T]]] with
//    override def apply(f: Format[T]): Format[Option[T]] =
//      new Format[Option[T]]:
//        override def format(value: Option[T]): String = f.format(value.get)

  // We can define Format of any kind of type for which there is a conversion from that type to T
  // This is even more general, A doesn't have to be a wrapper type
  // In the above example A is Option[T] and func is value.get
  // And I don't even need the method here because it was added to Format
  def contramap[A, T](func: A => T)(using f: Format[T]): Format[A] =
    new Format[A]:
      override def format(value: A): String = f.format(func(value))

  // So the optionFormat can be simplified
//  given getOptionFormat[T](using f: Format[T]): Conversion[Format[T], Format[Option[T]]] with
//    override def apply(f: Format[T]): Format[Option[T]] =
////      contramap(Option[T], T)(_.get)
//      f.contramap[Option[T]](_.get)

  // Has to be written like this because using Scala 3 implicit conversions doesn't work well with implicit arguments
  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

  /*
    How does compiler figure it out?
    1. We have an implicit Format[Int].
    2. fo: Format[Option[Int]] = Format[Int].contramap[Option[Int]](_.get) // first get
    3. fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]](_.get) // second get

    fo2 = IntFormat
      .contramap[Option[Int]](_.get)
      .contramap[Option[Option[Int]]](_.get)
    In the end this is the order of operations:
    fo2.format(Option(Option(42)) =
      fo1.format(secondGet(Option(Option(42))) =
      IntFormat.format(firstGet(secondGet(Option(Option(42))))

    Order - REVERSE (stack) from the written order:
    - second get
    - first get
    - format of Int

    Normal map of Functor applies transformations in sequence they are written, but contramap in reverse sequence
    Therefore, type classes that do something to a value and have a contramap method, are Contravariant Type Classes
   */

  import cats.Contravariant
  import cats.Show
  import cats.instances.int.given // given Show[Int]
  val showInts: Show[Int] = Show[Int]
  val showOptions: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  // I can decorate existing type classes with the contramap method via extension methods
  import cats.syntax.contravariant.given
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Nothing weird so far"))
    println(format(42))
    println(format(true))
    println(format(Option(42)))
    println(format(Option(Option(42))))
  }
