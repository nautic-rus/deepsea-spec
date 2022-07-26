import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.database.DatabaseManager.OracleConnection
import deepsea.pipe.PipeHelper
import org.mongodb.scala.MongoClient
import org.scalatest.funsuite.AnyFunSuite

class SpoolsReportTest extends AnyFunSuite with PipeHelper{
  val docNumber = "210101-701-0001"
  val docName = "Fuel System"
  val revision = "0"

  val configOracle = new HikariConfig()
  configOracle.setDriverClassName("oracle.jdbc.driver.OracleDriver")
  configOracle.setJdbcUrl("jdbc:oracle:thin:@office.nautic-rus.ru:1521:ORA3DB")
  configOracle.setUsername("C" + "N004")
  configOracle.setPassword("Whatab0utus")
  configOracle.setMaximumPoolSize(5)
  val ds = new HikariDataSource(configOracle)

  val mongoClient = MongoClient("mongodb://192.168.1.26")
  val projectSystem = getSystemAndProjectFromDocNumber(docNumber, mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry), ds.getConnection)
  val pipeSegs = getPipeSegsFromMongo(projectSystem._1, projectSystem._2, -1, mongoClient.getDatabase("cache").withCodecRegistry(codecRegistry), mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry))

  val hh=0
}
