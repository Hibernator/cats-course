package part2abstractmath

import java.util.concurrent.Executors
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

object UsingMonads {

  import cats.Monad
  import cats.instances.list.given
  import cats.syntax.flatMap.given
  import cats.syntax.functor.given

  val monadList = Monad[List] // fetch implicit Monad[List]

  val aSimpleList = monadList.pure(2) // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1))

  // all of above is applicable to Option, Try, Future...

  // Either is also a Monad
  val aManualEither: Either[String, Int] = Right(42)

  // Usually, right is the desirable value, therefore the following type aliases are often seen
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T] = Either[Throwable, T]

  import cats.instances.either.given

  // Cats can create a monad for an either that has left-hand type as a concrete type
  val loadingMonad = Monad[LoadingOr]

  val anEither = loadingMonad.pure(45) // LoadingOr[Int] == Right(45)
  val aChangedLoading =
    loadingMonad.flatMap(anEither)(n => if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of life..."))

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(OrderStatus(orderId, "Ready to ship"))
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data...")
    else Right("Amsterdam, NL")

  val orderId = 457L
  val orderLocation = loadingMonad.flatMap(getOrderStatus(orderId))(trackLocation)

  // use extension methods

  val orderLocationBetter: LoadingOr[String] = getOrderStatus(orderId).flatMap(trackLocation)
  val orderLocationFor: LoadingOr[String] = for {
    orderStatus <- getOrderStatus(orderId)
    location <- trackLocation(orderStatus)
  } yield location

  // Exercise: the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "4040"
  )

  // return types are very general and allow combining both methods with a for-comprehension
  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  // very general method, no matter the specific implementation of HttpService
  // I just need a monad in scope
  def getResponse[M[_]: Monad](service: HttpService[M], payload: String): M[String] =
    for {
      conn <- service.getConnection(config)
      response <- service.issueRequest(conn, payload)
    } yield response

  /*
    Requirements:
    - if the host and port are found in the configuration map, then we'll return a M containing a connection with those
      values, otherwise the method will fail, according to the logic of the type M
      (for Try it will return a Failure, for Option it will return None, for Future it will be a failed Future,
      for Either it will be Left)
    - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload
      is less than 20 characters, otherwise the method will fail, according to the logic of the type M

    Provide a real implementation of HttpService using Try, Option, Future, Either
   */

  object TryHttpService extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] = for {
      host <- Try(cfg("host"))
      port <- Try(cfg("port"))
    } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Try[String] =
      if (payload.length < 20) Success(s"request ($payload) has been accepted")
      else Failure(new RuntimeException("payload too long"))
  }

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] = for {
      host <- cfg.get("host")
      port <- cfg.get("port")
    } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] =
      Option.when(payload.length < 20)(s"request ($payload) has been accepted")
  }

  val executor = Executors.newFixedThreadPool(20)
  given ExecutionContext = ExecutionContext.fromExecutorService(executor)

  object FutureHttpService extends HttpService[Future] {
    override def getConnection(cfg: Map[String, String]): Future[Connection] =
      for {
        host <- Future(cfg("host"))
        port <- Future(cfg("port"))
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Future[String] =
      if (payload.length < 20) Future.successful(s"request ($payload) has been accepted")
      else Future.failed(new RuntimeException("payload too long"))
  }

  object LoadingOrHttpService extends HttpService[LoadingOr] {
    override def getConnection(cfg: Map[String, String]): LoadingOr[Connection] =
      for {
        host <- cfg.get("host").toRight("No host config")
        port <- cfg.get("port").toRight("No port config")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): LoadingOr[String] =
      Either.cond(payload.length < 20, s"request ($payload) has been accepted", "payload too long")
  }

  object ErrorOrHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      for {
        host <- cfg.get("host").toRight(new RuntimeException("No host config"))
        port <- cfg.get("port").toRight(new RuntimeException("No port config"))
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] =
      Either.cond(
        payload.length < 20,
        s"request ($payload) has been accepted",
        new RuntimeException("payload too long")
      )
  }

  def main(args: Array[String]): Unit = {
    val emptyConfig = Map[String, String]()
    val configWithoutPort = Map("host" -> "someHost")

    println(TryHttpService.getConnection(config))
    println(TryHttpService.getConnection(emptyConfig))
    println(TryHttpService.getConnection(configWithoutPort))
    println(TryHttpService.issueRequest(null, "short payload"))
    println(TryHttpService.issueRequest(null, "veeeeeeeeeeeeeeeeery long payload"))
    println

    println(OptionHttpService.getConnection(config))
    println(OptionHttpService.getConnection(emptyConfig))
    println(OptionHttpService.getConnection(configWithoutPort))
    println(OptionHttpService.issueRequest(null, "short payload"))
    println(OptionHttpService.issueRequest(null, "veeeeeeeeeeeeeeeeery long payload"))
    println

    FutureHttpService.getConnection(config).onComplete {
      case Success(connection) => println(connection)
      case Failure(ex)         => println(ex)
    }
    FutureHttpService.getConnection(emptyConfig).onComplete {
      case Success(connection) => println(connection)
      case Failure(ex)         => println(ex)
    }
    FutureHttpService.getConnection(configWithoutPort).onComplete {
      case Success(connection) => println(connection)
      case Failure(ex)         => println(ex)
    }
    FutureHttpService.issueRequest(null, "short payload").onComplete {
      case Success(value)     => println(value)
      case Failure(exception) => println(exception)
    }
    FutureHttpService.issueRequest(null, "veeeeeeeeeeeeeeeeery long payload").onComplete {
      case Success(value)     => println(value)
      case Failure(exception) => println(exception)
    }
    println

    println(LoadingOrHttpService.getConnection(config))
    println(LoadingOrHttpService.getConnection(emptyConfig))
    println(LoadingOrHttpService.getConnection(configWithoutPort))
    println(LoadingOrHttpService.issueRequest(null, "short payload"))
    println(LoadingOrHttpService.issueRequest(null, "veeeeeeeeeeeeeeeeery long payload"))
    println

    println(ErrorOrHttpService.getConnection(config))
    println(ErrorOrHttpService.getConnection(emptyConfig))
    println(ErrorOrHttpService.getConnection(configWithoutPort))
    println(ErrorOrHttpService.issueRequest(null, "short payload"))
    println(ErrorOrHttpService.issueRequest(null, "veeeeeeeeeeeeeeeeery long payload"))
    println

    println(getResponse(TryHttpService, "short payload"))
    println(getResponse(TryHttpService, "veeeeeeeeeeeeeeeeery long payload"))
    println

    println(getResponse(OptionHttpService, "short payload"))
    println(getResponse(OptionHttpService, "veeeeeeeeeeeeeeeeery long payload"))
    println

    println(getResponse(FutureHttpService, "short payload"))
    println(getResponse(FutureHttpService, "veeeeeeeeeeeeeeeeery long payload"))
    println

    println(getResponse(LoadingOrHttpService, "short payload"))
    println(getResponse(LoadingOrHttpService, "veeeeeeeeeeeeeeeeery long payload"))
    println

    println(getResponse(ErrorOrHttpService, "short payload"))
    println(getResponse(ErrorOrHttpService, "veeeeeeeeeeeeeeeeery long payload"))
    println

//    executor.shutdown()
  }
}
