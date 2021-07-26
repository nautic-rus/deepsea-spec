package deepsea.database

import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.actors.ActorManager
import deepsea.actors.ActorStartupManager.DatabaseManagerStarted
import deepsea.database.DatabaseManager.{GetConnectionFromPool, ReleaseConnectionFromPool}

import java.sql.Connection
import java.util.concurrent.TimeUnit
import scala.concurrent.Await

object DatabaseManager{
  implicit val timeout: Timeout = Timeout(5, TimeUnit.SECONDS)
  case class GetConnectionFromPool()
  case class ReleaseConnectionFromPool(connection: Connection)
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
  def ReleaseConnection(connection: Connection){
    ActorManager.dataBase ! ReleaseConnectionFromPool(connection)
  }
}
class DatabaseManager extends Actor{
  private val config = new HikariConfig()
  private var ds: HikariDataSource = _

  override def preStart(): Unit = {
    config.setDriverClassName("org.postgresql.Driver")
    config.setJdbcUrl("jdbc:postgresql://localhost/deepsea")
    config.setUsername("deepsea")
    config.setPassword("Ship1234")
    ds = new HikariDataSource(config)
    ActorManager.startup ! DatabaseManagerStarted()
  }
  override def receive: Receive = {
    case GetConnectionFromPool() => sender() ! ds.getConnection
    case ReleaseConnectionFromPool(connection: Connection) => ds.evictConnection(connection)
    case _ => None
  }
}
