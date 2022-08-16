package deepsea.database

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.database.DatabaseManager.{GetConnectionFromPool, GetMongoCacheConnectionFromPool, GetMongoConnectionFromPool, GetOracleConnectionFromPool, OracleConnection, timeout}
import local.common.Codecs
import org.mongodb.scala.{MongoClient, MongoDatabase}
import java.sql.Connection
import scala.collection.mutable.ListBuffer

object DBManager extends Codecs{

  private val configOracle = new HikariConfig()
  private val oracleConnections = ListBuffer.empty[OracleConnection]
  private val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26")

  List("N002", "N004").foreach(project => {
    configOracle.setDriverClassName("oracle.jdbc.driver.OracleDriver")
    configOracle.setJdbcUrl("jdbc:oracle:thin:@office.nautic-rus.ru:1521:ORA3DB")
    configOracle.setUsername("C" + project)
    configOracle.setPassword("Whatab0utus")
    configOracle.setMaximumPoolSize(5)
    oracleConnections += OracleConnection(project, new HikariDataSource(configOracle))
  })

  def GetOracleConnection(project: String): Option[Connection] ={
    oracleConnections.find(_.project == project) match {
      case Some(connection) => Option(connection.ds.getConnection)
      case _ => Option.empty
    }
  }
  def GetMongoConnection(): Option[MongoDatabase] = {
    Option(mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry))
  }
  def GetMongoCacheConnection(): Option[MongoDatabase] = {
    Option(mongoClient.getDatabase("cache").withCodecRegistry(codecRegistry))
  }
}
