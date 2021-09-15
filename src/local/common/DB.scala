package local.common

import local.common.DB.MongoDB
import local.domain.WorkShopMaterial
import local.hull.BStree.{BsTreeItem, HullPL}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoClient, MongoDatabase}
import org.mongodb.scala.bson.codecs.Macros._

import java.sql.{Connection, DriverManager}

object DB {
  case class MongoDB(mongoClient: MongoClient, mongoDatabase: MongoDatabase)
  case class Room(num:String,name:String)
}

trait DB {

  def oracleConnection(): Option[Connection] = {

    try {
      val oraConnString: String = s"jdbc:oracle:thin:@office.nautic-rus.ru:1521:ORA2DB"
      val conn: Connection = DriverManager.getConnection(oraConnString, "CN002", "Whatab0utus")
      Option(conn)
    } catch {
      case _: Throwable => {
        None
      }
    }
  }

  def configureMongoDB(): MongoDB = {
    val codecRegistry: CodecRegistry =
      fromRegistries(fromProviders(
        classOf[WorkShopMaterial],
        classOf[HullPL],
        classOf[BsTreeItem]
      ), DEFAULT_CODEC_REGISTRY)
    val mongoClient: MongoClient = MongoClient("mongodb://office.nautic-rus.ru:20120")
    val mongoDatabase: MongoDatabase = mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    MongoDB(mongoClient, mongoDatabase)
  }

}
