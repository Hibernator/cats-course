package part5alien

import cats.kernel.Monoid

object InvariantFunctors:
  // Nothing to do with generic type variance

  // Example: type class that can encrypt and decrypt values

  /** Can turn a value of type A into a String and back
    * @tparam A
    *   type of the value
    */
  trait Crypto[A]:
    self =>
    def encrypt(value: A): String // similar to format in Contravariant Functors lesson
    def decrypt(encrypted: String): A

    // Similar to contramap
    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B]:
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))

  def encrypt[A](value: A)(using crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](repr: String)(using crypto: Crypto[A]): A = crypto.decrypt(repr)

  given caesarCypher: Crypto[String] = new Crypto[String]:
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)

    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)

  /*
    I'd like to reuse the logic for other types: ints, doubles, Option[String]
    How can we support this if we have a two-way conversion logic between above types and string?
    Add an imap method to the Crypto type class
   */

  given intCrypto: Crypto[Int] = caesarCypher.imap(_.toString, _.toInt)
  given doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  // Exercise 1: support Option[String]
  given optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))

  // Exercise 2: generalize the pattern above
  // If you have a Crypto[T], derive Crypto[Option[T]] if there is a Monoid[T] in scope
  given optionAnyCrypto[T](using cryptoT: Crypto[T], monoidT: Monoid[T]): Crypto[Option[T]] =
    cryptoT.imap(_.getOrElse(monoidT.empty), Option(_))

  // Invariant in cats
  import cats.Invariant
  import cats.Show

  import cats.instances.string.given // given Show[String]
  val showString: Show[String] = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  // extension methods
  import cats.syntax.invariant.given
  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse(""))

  // Exercise 3: establishing relationship between Invariant and Contravariant
  // Establish the relationship between MyInvariant, MyContravariant and MyFunctor
  // by implementing one or two of the below methods in terms of one another
  // Who extends who? Subclass is stronger and more restrictive than superclass
  // and can implement the weaker (less restrictive) method of superclass
  // in terms of its own stronger (restrictive) method
  trait MyInvariant[W[_]]:
    // fundamental Invariant method
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]

  trait MyContravariant[W[_]] extends MyInvariant[W]:
    def contramap[A, B](wa: W[A])(back: B => A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = contramap(wa)(back)

  // "Convariant" Functor
  trait MyFunctor[W[_]] extends MyInvariant[W]:
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B] = map(wa)(forth)

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Let's encrypt")
    val decrypted = decrypt[String](encrypted)
    println(encrypted)
    println(decrypted)

    val encryptedInt = encrypt(42)
    val decryptedInt = decrypt[Int](encryptedInt)
    println(encryptedInt)
    println(decryptedInt)

    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))

    println(encrypt(Option("Let's encrypt")))
    println(decrypt[Option[String]](encrypted)) // I can actually pass a string as well, not an Option
    println(encrypt(Option.empty[String]))
    println(decrypt[Option[String]](encrypt(Option.empty[String])))

    import cats.instances.double.given // given Monoid[Double]
    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Option(Math.PI))))
    println(encrypt(Option.empty[Double]))
    println(decrypt[Option[Double]](encrypt(Option.empty[Double])))
  }
