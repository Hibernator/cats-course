package part2abstractmath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}

object MonadTransformers {

  // normally, I would need to unwrap the options but since it's a nested monad, I can avoid this
  // monad transformers allow using map and flatMap on nested monads without unwrapping them
  // it's basically a conveniency API over nested monadic values
  def sumAllOptions(values: List[Option[Int]]): Int = ???

  // Option transformer
  import cats.data.OptionT // OptionTransformer
  import cats.instances.list.given // fetch an implicit OptionT[List]. The list will contain options in our case

  // the type means: list of Options of Int (middle, left, right)
  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char] = OptionT(List(Option('a'), Option('b'), Option.empty))
  // to create combinations of tuples from the lists above, I would need to do a lot of unwrapping

  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)
  // I have a wrapper over the outer monad which saves me unwrapping of the inner monad

  // Either transformer
  import cats.data.EitherT

  val listOfEithers: EitherT[List, String, Int] = EitherT(List(Left("something wrong"), Right(43), Right(2)))
  given ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT(Future(Right(45)))
  val futureOfEitherShort: EitherT[Future, String, Int] = EitherT.right(Future(45)) // wrap over Future(Right(45))

  /*
    Exercise:
    We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
    We measure bandwidth in units.
    We want to allocate TWO of our serves to cope with the traffic surge.
    We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is > 250.
   */
  val bandwidths = Map(
    "server1.rockthejvm.com" -> 50,
    "server2.rockthejvm.com" -> 300,
    "server3.rockthejvm.com" -> 170
  )
  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None    => EitherT.left(Future(s"Server $server unreachable"))
    case Some(b) => EitherT.right(Future(b))
  }

  // TO DO 1
  // Returns Right(true) if two servers have bandwidth over 250
  // Hint: call getBandwidth twice, and combine the results
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    bandwidth1 <- getBandwidth(s1)
    bandwidth2 <- getBandwidth(s2)
  } yield bandwidth1 + bandwidth2 > 250 // Future[Either[String, Boolean]]

  // TO DO 2
  // Returns Right("servers support surge") or Left with explanation why not (either server unreachable or bandwidth too low)
  // Hint: call canWithstandSurge + transform
  import cats.instances.future.given
  // transform method changes the deeply nested Either without unwrapping the outer monad
  def generateTrafficSpikeReport(s1: String, s2: String): AsyncResponse[String] = canWithstandSurge(s1, s2).transform {
    case Right(false) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: not enough total bandwidth")
    case Right(true)  => Right(s"Servers $s1 and $s2 can cope with the incoming spike NO PROBLEM")
    case Left(error)  => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: $error")
  }
  // ^^^^^^^^^^^^^^^                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  // Future[Either[String, Boolean]]  ---- Future[Either[String, String]]

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)
    canWithstandSurge("server1.rockthejvm.com", "server2.rockthejvm.com").value.foreach(println)
    canWithstandSurge("server1.rockthejvm.com", "server3.rockthejvm.com").value.foreach(println)
    canWithstandSurge("server4.rockthejvm.com", "server5.rockthejvm.com").value.foreach(println)
    val goodResultFuture = generateTrafficSpikeReport("server1.rockthejvm.com", "server2.rockthejvm.com").value
    goodResultFuture.foreach(println)
    val insufficientBandwidthResultFuture =
      generateTrafficSpikeReport("server1.rockthejvm.com", "server3.rockthejvm.com").value
    insufficientBandwidthResultFuture.foreach(println)
    val unreachableServerResultFuture =
      generateTrafficSpikeReport("server5.rockthejvm.com", "server3.rockthejvm.com").value
    unreachableServerResultFuture.foreach(println)
  }
}
