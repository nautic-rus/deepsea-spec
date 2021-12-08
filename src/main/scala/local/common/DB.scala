package local.common


import local.domain.WorkShopMaterial
import local.hull.BStree.{BsTreeItem, HullPL}
import org.apache.log4j.{Level, Logger}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoClient, MongoDatabase}
import org.mongodb.scala.bson.codecs.Macros._

import java.sql.{Connection, DriverManager}

object DB {
 // case class MongoDB(mongoClient: MongoClient, mongoDatabase: MongoDatabase)

}

trait DB {

  def oracleConnection(project:String="N002"): Option[Connection] = {

    try {
      val oraConnString: String = s"jdbc:oracle:thin:@office.nautic-rus.ru:1521:ORA2DB"

      val pass:String= project match {
        case "N004"=>"Whatab0utus"
        case "N002"=>"Whatab0utus"
        case "P701"=>"ThisIsH0wWeD0"
        case "P702"=>"ThisIsH0wWeD0"
        case _=>""
      }

      val conn: Connection = DriverManager.getConnection(oraConnString,s"C${project}" , pass)
      Option(conn)
    } catch {
      case x: Throwable => {
        println(x.toString)
        None
      }
    }
  }

/*  def configureMongoDB(): MongoDB = {
    Logger.getLogger("org.mongodb.driver").setLevel(Level.ERROR)
    val codecRegistry: CodecRegistry =
      fromRegistries(fromProviders(
        classOf[WorkShopMaterial],
        classOf[HullPL],
        classOf[BsTreeItem]
      ), DEFAULT_CODEC_REGISTRY)
    val mongoClient: MongoClient = MongoClient("mongodb://office.nautic-rus.ru:20120")
    val mongoDatabase: MongoDatabase = mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    MongoDB(mongoClient, mongoDatabase)
  }*/

}
