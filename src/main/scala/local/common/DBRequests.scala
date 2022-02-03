package local.common

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import local.domain.{CommonSql, WorkShopMaterial}
import local.domain.CommonTypes.{DrawingChess, ZoneSystem}
import local.sql.{ConnectionManager, MongoDB}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoCollection, MongoDatabase}
import org.mongodb.scala.model.Filters.equal
import org.mongodb.scala.model.Filters.and
import org.mongodb.scala.bson.codecs.Macros._

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._


object DBRequests extends Codecs {

  case class MountItem(workShopMaterial: WorkShopMaterial = new WorkShopMaterial(), label: String = "NF", kei: String = "", qty: Double = 0, isNeedLabel: Boolean = false)

  private def collectionWorkShopMaterial(): MongoCollection[WorkShopMaterial] = mongoDatabase().getCollection("materials")

  private def collectionDrawingChess(): MongoCollection[DrawingChess] = mongoDatabase().getCollection("drawingChess")

/*
  private val duration: FiniteDuration = Duration(2, SECONDS)

  private val codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[WorkShopMaterial],
  ), DEFAULT_CODEC_REGISTRY)
*/


  def retrieveZoneAndSystems(project: String): List[ZoneSystem] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[ZoneSystem]
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(CommonSql.zonesAndSystemsSql)
          while (rs.next()) {
            buffer += ZoneSystem(
              Option(rs.getString("TYPENAME")).getOrElse("zone"),
              Option(rs.getInt("OID")).getOrElse(0),
              Option(rs.getInt("SEQID")).getOrElse(0),
              Option(rs.getString("USERID")).getOrElse(""),
              Option(rs.getString("NAME")).getOrElse(""),
              Option(rs.getString("DESCR_RU")).getOrElse(""),
              Option(rs.getString("DESCR_EN")).getOrElse("")
            )
          }
          stmt.close()
          connection.close()
          buffer.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[ZoneSystem]
        }
      }
      case None => List.empty[ZoneSystem]
    }
  }

  def retrieveAllMaterials(): List[WorkShopMaterial] = {
    val allelems = collectionWorkShopMaterial().find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    allelems.value.get.getOrElse(Seq.empty[WorkShopMaterial]).toList
  }

  def retrieveAllMaterialsByProject(project: String): List[WorkShopMaterial] = {
    val allelems = collectionWorkShopMaterial().find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    allelems.value.get.getOrElse(Seq.empty[WorkShopMaterial]).toList
  }

  def findWorkshopMaterial(trm: String, buff: List[WorkShopMaterial]): WorkShopMaterial = buff.find(s => s.trmCode.equals(trm)).getOrElse(new WorkShopMaterial())


  def findChess(docNum: String, rev:String): List[DrawingChess] = {
    val allelems = collectionDrawingChess().find(
      and(
        equal("docNumber",docNum),
        equal("revision",rev)
      )
    ).toFuture()

    //val allelems = collectionDrawingChess().find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    allelems.value.get.getOrElse(Seq.empty[DrawingChess]).toList
  }

  def findWorkshopMaterialContains(trm: String, buff: List[WorkShopMaterial]): WorkShopMaterial = buff.find(s => s.trmCode.contains(trm)).getOrElse(new WorkShopMaterial())

  def findWorkshopMaterialEquals(trm: String, buff: List[WorkShopMaterial]): WorkShopMaterial = buff.find(s => s.trmCode.equals(trm)).getOrElse(new WorkShopMaterial())


  def calculateH(X_COG: Double = 0, Y_COG: Double = 0, Z_COG: Double = 0, SURFACE: String = ""): Int = {
    if (SURFACE.nonEmpty) {
      SURFACE.head.toUpper.toString match {
        case "X" => Math.abs(Math.abs(X_COG.toInt) - Math.abs(getCoordFromBSString(SURFACE)))
        case "Y" => Math.abs(Math.abs(Y_COG.toInt) - Math.abs(getCoordFromBSString(SURFACE)))
        case "Z" => Math.abs(Math.abs(Z_COG.toInt) - Math.abs(getCoordFromBSString(SURFACE)))
        case "F" => Math.abs(getCoordFromBSString(SURFACE))
        case _ => 0
      }
    } else {
      0
    }
  }

  private def getCoordFromBSString(inp: String): Int = {
    val in = inp.replace("-", "")
    if (in.nonEmpty && in.length > 2) {
      if (in.contains(";")) {
        in.split(";").headOption match {
          case Some(value) => value.toIntOption.getOrElse(0)
          case None => 0
        }
      } else {
        in.tail.toIntOption.getOrElse(0)
      }
    }
    else 0
  }

  def listToSqlString(in: List[String]): String = {
    if (in.nonEmpty) {
      var ret = ""
      in.foreach(s => {
        ret += "'" + s + "',"
      })
      ret.dropRight(1)
    } else {
      "'0'"
    }
  }
}
