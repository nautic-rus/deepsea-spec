package local.sql

import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.App
import org.apache.log4j.{Level, Logger}
import org.mongodb.scala.{MongoClient, MongoDatabase}

import java.sql.{Connection, DriverManager}
import java.util.TimeZone
import scala.collection.mutable.ListBuffer

object ConnectionManager {
  Class.forName("oracle.jdbc.driver.OracleDriver")

  private val debugLevel: Level =Level.OFF

  case class ForanConnection(project: String, ds: HikariDataSource)

  private var ds: HikariDataSource = _

  private lazy val connections: List[ForanConnection] = {
    val buff = ListBuffer.empty[ForanConnection]
    DriverManager.setLoginTimeout(App.conf.getInt("oracle.logintimeout"))
    TimeZone.setDefault(TimeZone.getTimeZone("Europe/Moscow"))
    App.conf.getObjectList("foranOracle").forEach(conf => {
      val config = new HikariConfig
      val Appconf = conf.toConfig
      val connName = Appconf.getString("login")
      config.setJdbcUrl(s"jdbc:oracle:thin:@${Appconf.getString("host")}:${Appconf.getString("port")}/${Appconf.getString("database")}")
      config.setUsername(Appconf.getString("login"))
      config.setPassword(Appconf.getString("password"))
      config.addDataSourceProperty("v$session.osuser", App.conf.getString("app.user"))
      config.addDataSourceProperty("v$session.machine", App.conf.getString("app.machine"))
      config.addDataSourceProperty("v$session.program", App.conf.getString("app.program"))
      config.addDataSourceProperty("v$session.terminal", App.conf.getString("app.machine"))
      config.setConnectionTestQuery("SELECT 1 FROM DUAL")
      config.setConnectionTimeout(2000)
      config.setMaximumPoolSize(3)
      config.setMaxLifetime(1800000)
      config.setMinimumIdle(20)
      config.setValidationTimeout(3000)
      config.setIdleTimeout(1500)
      config.setAutoCommit(false)
      config.setLeakDetectionThreshold(1800000)
      config.setPoolName(connName + "-pool")
      Logger.getLogger("com.zaxxer.hikari.pool.PoolBase").setLevel(debugLevel)
      Logger.getLogger("com.zaxxer.hikari.pool.HikariPool").setLevel(debugLevel)
      Logger.getLogger("com.zaxxer.hikari.HikariDataSource").setLevel(debugLevel)
      Logger.getLogger("com.zaxxer.hikari.HikariConfig").setLevel(debugLevel)
      Logger.getLogger("com.zaxxer.hikari.util.DriverDataSource").setLevel(debugLevel)
      buff += ForanConnection(connName, new HikariDataSource(config))
    })
    buff.toList
  }

  def init(): Unit = {
    DriverManager.setLoginTimeout(App.conf.getInt("oracle.logintimeout"))
    TimeZone.setDefault(TimeZone.getTimeZone("Europe/Moscow"))
    val config = new HikariConfig
    config.setJdbcUrl(s"jdbc:oracle:thin:@${App.conf.getString("oracle.host")}:${App.conf.getString("oracle.port")}/${App.conf.getString("oracle.database")}")
    config.setUsername(App.conf.getString("oracle.login"))
    config.setPassword(App.conf.getString("oracle.password"))
    config.setConnectionTestQuery("SELECT 1 FROM DUAL")
    config.setConnectionTimeout(600000)
    config.setMaximumPoolSize(500)
    config.setMaxLifetime(1800000)
    config.setMinimumIdle(20)
    config.setValidationTimeout(3000)
    config.setIdleTimeout(60000)
    config.setAutoCommit(false)
    config.setLeakDetectionThreshold(1800000)
    config.addDataSourceProperty("v$session.osuser", App.conf.getString("app.user"))
    config.addDataSourceProperty("v$session.machine", App.conf.getString("app.machine"))
    config.addDataSourceProperty("v$session.program", App.conf.getString("app.program"))
    config.addDataSourceProperty("v$session.terminal", App.conf.getString("app.machine"))
    ds = new HikariDataSource(config)

  }

  def getConnection: Connection = ds.getConnection

  def connectionByProject(projectName: String): Option[Connection] = connections.find(s => s.project.equals("C" + projectName.toUpperCase)) match {
    case Some(value) => Option[Connection](value.ds.getConnection())
    case None => None
  }

  def mongoClient(): MongoClient = MongoClient(s"mongodb://${App.conf.getString("mongo.host")}:${App.conf.getInt("mongo.port").toString}")

}