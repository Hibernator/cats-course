package part3datamanipulation

object Readers {

  /*
    - we have a multi-layered application
    - configuration file => contains info for initial data structure consisting of several layers (usernames, passwords)
    - DB layer
    - HTTP layer
    - business logic layer
   */

  // contains parameters useful for different layers of the app
  case class Configuration(
      dbUsername: String,
      dbPassword: String,
      host: String,
      port: Int,
      nThreads: Int,
      emailReplyTo: String
  )

  case class DbConnection(username: String, password: String) {
    // select * from the db table and return the status of the orderID
    def getOrderStatus(orderId: Long): String = "dispatched"
    def getLastOrderId(username: String): Long = 542643 // select max(orderId) from table where username = username
  }
  case class HttpService(host: String, port: Int) {
    // this would start the actual server
    def start(): Unit = println("server started")
  }

  // bootstrapping - read the config file and spin up various layers of the app
  val config: Configuration = Configuration("daniel", "rockthejvm1!", "localhost", 1234, 8, "daniel@rockthejvm.com")

  // cats Reader
  import cats.data.Reader
  // two type parameters: input and output
  // it's a specification of how to derive a DbConnection from a Configuration
  val dbReader: Reader[Configuration, DbConnection] = Reader(conf => DbConnection(conf.dbUsername, conf.dbPassword))
  val dbConn: DbConnection = dbReader.run(config)

  // Reader[I, O]
  // additional readers can be derived via a map method
  val danielsOrderStatusReader: Reader[Configuration, String] = dbReader.map(dbCon => dbCon.getOrderStatus(55))
  val danielsOrderStatus: String = danielsOrderStatusReader.run(config)

  def getLastOrderStatus(username: String): String = {
    val usersLastOrderIdReader: Reader[Configuration, String] =
      dbReader.map(_.getLastOrderId(username)).flatMap(lastOrderId => dbReader.map(_.getOrderStatus(lastOrderId)))

    // since we have map and flatMap, a for-comprehension can be used
    val usersOrderFor: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      orderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
    } yield orderStatus

    usersOrderFor.run(config)
  }

  // I can obtain a very specific piece of information based on the original config/data structure
  /*
    Pattern
    1. Create the initial data structure
    2. Create a reader that specifies how that data structure will be manipulated (transformed into something else)
    3. Map and flatMap the reader to produce derived information
    4. When the final piece of information is needed, call run on the reader with the initial data structure
   */

  // Exercise
  case class EmailService(emailReplyTo: String) {
    def sendEmail(address: String, contents: String): String = s"From: $emailReplyTo; to: $address >>> $contents"
  }

  // TO DO: 1 implement this method, email a user
  def emailUser(username: String, userEmail: String): String = {
    // fetch the status of their last order
    // email them with the EmailService: "Your order has the status: (status)
    val emailServiceReader: Reader[Configuration, EmailService] = Reader(conf => EmailService(conf.emailReplyTo))

    val sendEmailReader: Reader[Configuration, String] = for {
      lastOrderId <- dbReader.map(_.getLastOrderId(username))
      lastOrderStatus <- dbReader.map(_.getOrderStatus(lastOrderId))
      emailService <- emailServiceReader
    } yield emailService.sendEmail(userEmail, s"Your order has the status: $lastOrderStatus")

    sendEmailReader.run(config)
  }

  // TO DO 2: what programming pattern do Readers remind you of?
  // Dependency injection - in a purely functional way, because the initial data is injected only when I run the flow

  def main(args: Array[String]): Unit = {
    println(getLastOrderStatus("daniel"))
    println(emailUser("Oleg", "gibernator@gmail.com"))
  }
}
