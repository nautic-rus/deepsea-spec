package deepsea.database

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.DatabaseManagerStarted
import deepsea.database.DatabaseManager.{GetConnectionFromPool, GetMongoCacheConnectionFromPool, GetMongoConnectionFromPool, GetOracleConnectionFromPool, OracleConnection}
import local.common.Codecs
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
  case class GetMongoCacheConnectionFromPool()
  case class GetConnectionFromPool()

  def GetConnection(): Option[Connection] ={
    try{
      Await.result(ActorManager.dataBase ? GetConnectionFromPool(), timeout.duration) match {
        case response: Connection => Option(response)
        case _ => Option.empty
      }
    }
    catch {
      case e: Throwable => Option.empty
    }
  }
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
  def GetMongoCacheConnection(): Option[MongoDatabase] = {
    try{
      Await.result(ActorManager.dataBase ? GetMongoCacheConnectionFromPool(), timeout.duration) match {
        case response: MongoDatabase => Option(response)
        case _ => Option.empty
      }
    }
    catch {
      case e: Throwable => Option.empty
    }
  }
}

class DatabaseManager extends Actor with Codecs {

  private val config = new HikariConfig()
  private var ds: HikariDataSource = _

  private val configOracle = new HikariConfig()
  private val oracleConnections = ListBuffer.empty[OracleConnection]

  private var mongoClient: MongoClient = _


  override def preStart(): Unit = {

    config.setDriverClassName("org.postgresql.Driver")
    config.setJdbcUrl("jdbc:postgresql://192.168.1.26/deepsea")
    config.setUsername("deepsea")
    config.setPassword("Ship1234")
    ds = new HikariDataSource(config)

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
    case GetConnectionFromPool() => sender() ! ds.getConnection
    case GetOracleConnectionFromPool(project) =>
      oracleConnections.find(_.project == project) match {
        case Some(connection) => sender() ! connection.ds.getConnection
        case _ => Option.empty
      }
    case GetMongoConnectionFromPool() => sender() ! mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    case GetMongoCacheConnectionFromPool() => sender() ! mongoClient.getDatabase("cache").withCodecRegistry(codecRegistry)

    case _ => None
  }
}
