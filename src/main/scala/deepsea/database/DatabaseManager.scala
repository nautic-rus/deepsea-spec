package deepsea.database

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.DatabaseManagerStarted
import deepsea.database.DatabaseManager.{GetMongoConnectionFromPool, GetOracleConnectionFromPool, OracleConnection, codecRegistry}
import org.mongodb.scala.{MongoClient, MongoDatabase}

import java.sql.Connection
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await

object DatabaseManager{

  implicit val timeout: Timeout = Timeout(5, TimeUnit.SECONDS)
  case class GetOracleConnectionFromPool(project: String)
  case class OracleConnection(project: String, ds: HikariDataSource)
  case class GetMongoConnectionFromPool()

  def GetOracleConnection(project: String): Option[Connection] ={
    try{
      Await.result(ActorManager.dataBase ? GetOracleConnectionFromPool(project), timeout.duration) match {
        case response: Connection => Option(response)
        case _ => Option.empty
      }
    }
    catch {
      case e: Throwable => Option.empty
    }
  }
  def GetMongoConnection(): Option[MongoDatabase] = {
    try{
      Await.result(ActorManager.dataBase ? GetMongoConnectionFromPool(), timeout.duration) match {
        case response: MongoDatabase => Option(response)
        case _ => Option.empty
      }
    }
    catch {
      case e: Throwable => Option.empty
    }
  }
}

class DatabaseManager extends Actor with MongoCodecs {


  private val configOracle = new HikariConfig()
  private val oracleConnections = ListBuffer.empty[OracleConnection]

  private var mongoClient: MongoClient = _


  override def preStart(): Unit = {

    List("P701", "P707", "N002", "N003", "N004").foreach(project => {
      configOracle.setDriverClassName("oracle.jdbc.driver.OracleDriver")
      configOracle.setJdbcUrl("jdbc:oracle:thin:@office.nautic-rus.ru:1521:ORA3DB")
      configOracle.setUsername("C" + project)
      configOracle.setPassword("Whatab0utus")
      configOracle.setMaximumPoolSize(5)
      oracleConnections += OracleConnection(project, new HikariDataSource(configOracle))
    })

    mongoClient = MongoClient("mongodb://192.168.1.26")


    ActorManager.startup ! DatabaseManagerStarted()
  }
  override def receive: Receive = {
    case GetOracleConnectionFromPool(project) =>
      oracleConnections.find(_.project == project) match {
        case Some(connection) => sender() ! connection.ds.getConnection
        case _ => Option.empty
      }
    case GetMongoConnectionFromPool() => sender() ! mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)

    case _ => None
  }
}
