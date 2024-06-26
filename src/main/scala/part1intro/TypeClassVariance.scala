package part1intro

object TypeClassVariance {

//  import cats.Eq
  import cats.syntax.eq._ // Eq[Int] type class instance
  import cats.instances.int.given // Eq[Option[Int]] type class instance
  import cats.instances.option.given // extension methods of Eq

  val aComparison = Option(2) === Option(3)

  // compiler can't find an instance of Eq[Some[Int]], it's not a subtype of Eq[Option[Int]], not covariant
//  val anInvalidComparison = Some(2) === None

  // variance
  class Animal
  class Cat extends Animal

  // covariant type: subtyping is propagated to the generic type
  class Cage[+T]
  val cage: Cage[Animal] = new Cage[Cat] // Cat <: Animal, so Cage[Cat] <: Cage[Animal]

  // contravariant type: subtyping is propagated BACKWARDS to the generic type
  class Vet[-T]
  // I want a vet of cat, but got even a better one, which can heal any animal
  val vet: Vet[Cat] = new Vet[Animal] // Cat <: Animal, then Vet[Animal] <: Vet[Cat]

  // rule of thumb: "HAS a T" => convariant, "ACTS on T" => contravariant
  // variance affect how type class instances are being fetched

  // contravariant TC
  trait SoundMaker[-T]
  given animalSoundMaker: SoundMaker[Animal] with {}

  def makeSound[T](using soundMaker: SoundMaker[T]): Unit = println("wow")
  makeSound[Animal] // ok - TC instance is defined above
  makeSound[Cat] // ok - TC instance for Animal is also applicable to Cats (in fact it's better)
  // rule 1: contravariant TCs can use the superclass instances if nothing is available strictly for that type

  // has implications for subtypes
  given optionSoundMaker: SoundMaker[Option[Int]] with {}
  makeSound[Option[Int]]
  // since SoundMaker is contravariant, the instance of SoundMaker[Option[Int]] can support Some as well
  makeSound[Some[Int]]

  // covariant TC
  trait AnimalShow[+T] {
    def show: String
  }

  given generalAnimalShow: AnimalShow[Animal] with {
    override def show: String = "animals everywhere"
  }

  given catsShow: AnimalShow[Cat] with {
    override def show: String = "so many cats"
  }

  def organizeShow[T](using event: AnimalShow[T]): String = event.show // 3rd TC part, API
  // rule 2: covariant TCs will always use the more specific TC instance for the type
  // may confuse the compiler if the general TC is also present (not really an issue in Scala 3)

  // rule 3: can't have both benefits
  // Cats library uses INVARIANT TCs

  // If I want to compare Some to None, do this. Use general type and smart constructors
  Option(2) === Option.empty[Int]

  def main(args: Array[String]): Unit = {
    println(organizeShow[Cat]) // ok - compiler will inject CatsShow as implicit
    println(organizeShow[Animal]) // shouldn't be OK but will still compile, using the most specific TC instance
    // wouldn't compile, if I added a dog TC instance as well

  }
}
