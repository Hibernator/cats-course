package part1recap

import scala.language.implicitConversions

object Implicits {

  // implicit classes, extension methods
  case class Person(name: String):
    def greet: String = s"Hi, my name is $name!"

  extension (name: String) {
    def greet: String = Person(name).greet
  }

  val greeting: String = "Peter".greet

  // importing implicit conversion in scope
  import scala.concurrent.duration._
  val oneSecond: FiniteDuration = 1.second

  // implicit arguments and values
  def increment(x: Int)(using amount: Int): Int = x + amount
  given Int = 10
  val incremented2: Int = increment(2) // implicit argument 10 is passed by compiler

  def multiply(x: Int)(using times: Int): Int = x * times
  val times2: Int = multiply(2) // same argument is passed as to increment

  // more complex example
  trait JSONSerializer[T]:
    def toJson(value: T): String

  def listToJson[T](list: List[T])(using serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  given JSONSerializer[Person] = new JSONSerializer[Person]:
    override def toJson(person: Person): String =
      s"""
         |{"name" : "${person.name}"}
         |""".stripMargin

  val personsJson: String = listToJson(List(Person("Alice"), Person("Bob")))
  // implicit argument is used to PROVE THE EXISTENCE of a type

  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T]:
    override def toJson(value: T): String =
      s"""
         |{"${value.productElementName(0)}" : "${value.productElement(0)}"}
         |""".stripMargin.trim

  case class Cat(name: String)
  val catsToJson: String = listToJson(List(Cat("Tom"), Cat("Garfield")))
  // in the background: val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))(oneArgCaseClassSerializer[Cat])
  // implicit methods are used to PROVE THE EXISTENCE of a type
  // can be used for implicit conversions (DISCOURAGED)

  def main(args: Array[String]): Unit = {
    println(oneArgCaseClassSerializer[Cat].toJson(Cat("Garfield")))
    println(oneArgCaseClassSerializer[Person].toJson(Person("David")))
  }

}
