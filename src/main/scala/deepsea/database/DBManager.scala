package deepsea.database

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import local.common.Codecs
import org.mongodb.scala.{MongoClient, MongoDatabase}

import java.sql.{Connection, ResultSet}
import scala.collection.mutable.ListBuffer

object DBManager extends Codecs{

  case class OracleConnection(project: String, ds: HikariDataSource)

  case class RsIterator(rs: ResultSet) extends Iterator[ResultSet] {
    def hasNext: Boolean = rs.next()
    def next(): ResultSet = rs
  }

  private val configOracle = new HikariConfig()
  private val oracleConnections = ListBuffer.empty[OracleConnection]
  private val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.36")

  List("N002", "N004", "N005", "N007", "SC01", "LV01", "AN01", "P701", "NT02", "KA01").foreach(project => {
    try{
        configOracle.setDriverClassName("oracle.jdbc.driver.OracleDriver")
        configOracle.setJdbcUrl("jdbc:oracle:thin:@192.168.1.12:1521:ORA3DB")
        configOracle.setUsername("C" + project)
        configOracle.setPassword("Whatab0utus")
        configOracle.setMaximumPoolSize(10)
        oracleConnections += OracleConnection(project, new HikariDataSource(configOracle))
    }
    catch {
      case e: Exception =>
    }
  })

  private val configPG = new HikariConfig()
  configPG.setDriverClassName("org.postgresql.Driver")
  configPG.setJdbcUrl("jdbc:postgresql://192.168.1.26/deepsea")
  configPG.setUsername("deepsea")
  configPG.setPassword("Ship1234")
  configPG.setMaximumPoolSize(5)
  val dsPG = new HikariDataSource(configPG)

  def GetOracleConnection(project: String): Option[Connection] ={
    oracleConnections.find(_.project == project) match {
      case Some(connection) =>
        try{
          Option(connection.ds.getConnection)
        }
        catch {
          case e: Exception => println(e.toString)
          Option.empty
        }
      case _ => Option.empty
    }
  }
  def GetMongoConnection(): Option[MongoDatabase] = {
    Option(mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry))
  }

  def GetMongoNewConnection(): Option[MongoDatabase] = {
    //Option(MongoClient("mongodb://192.168.1.36").getDatabase("3degdatabase").withCodecRegistry(codecRegistry))
    Option(mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry))
  }
  def GetMongoCacheConnection(): Option[MongoDatabase] = {
    Option(mongoClient.getDatabase("cache").withCodecRegistry(codecRegistry))
  }
  def GetPGConnection(): Option[Connection] = {
    Option(dsPG.getConnection)
  }
}
