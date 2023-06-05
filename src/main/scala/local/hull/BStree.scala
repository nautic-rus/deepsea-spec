package local.hull

import com.mongodb.BasicDBObject
import deepsea.database.DBManager
import io.circe.syntax.EncoderOps
import local.hull.BStree.{Block, BsTreeItem, HullPL, Room}
import org.bson.types.ObjectId

import java.sql.{ResultSet, Statement}
import java.util.Date
import scala.collection.mutable.ListBuffer
import local.common.Codecs
import local.sql.ConnectionManager.{connectionByProject, mongoClient}
import org.mongodb.scala.{Document, FindObservable, MongoCollection}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source


object BStree extends Codecs{
  case class Room(num:String,name:String)
  case class Block(
                    OID: Int,
                    CODE: String,
                    DESCRIPTION: String
                  )


  case class BsTreeItem(
                         PP_OID: Int,
                         EXPL_OID: Int,
                         BLOCK: String,
                         CODE: String,
                         PART_TYPE: Int,
                         DESCRIPTION: String,
                         PARENT_NODE: Int,
                         ATOM_TYPE: Int,
                         ORDINAL: Int,
                         TYPE_ATOM_TYPE: Int,
                         SYMBOL: String,
                         WEIGHT: Double,
                         X_COG: Double,
                         Y_COG: Double,
                         Z_COG: Double,
                         PRF_LENGTH: Double,
                         LINE_NAME: String,
                         LINE_TYPE: String,
                         LINE_TYPE_DESCRIPTION: String,
                         BS_ADN: String,
                         KSE: String,
                         STOCKCODE: String
                       )

  case class HullPL(
                     var _id: ObjectId,
                     project: String,
                     content: List[BsTreeItem],
                     rooms: List[Room],
                     var user: String,
                     var docNumber: String,
                     var revision: String,
                     var date: Long,
                     var version: Int
                   ) extends PartList

  trait PartList{
    var user: String
    var docNumber: String
    var revision: String
    var version: Int
    var date: Long
  }

}


trait BStree  extends Codecs{

  def genBlocks(project: String): String = {
    val items = ListBuffer.empty[Block]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val sql = "SELECT OID,CODE,DESCRIPTION FROM BLOCK order by code"
        val stmt: Statement = conn.createStatement()
        val rs: ResultSet = stmt.executeQuery(sql)
        while (rs.next()) {
          val OID: Int = Option(rs.getInt("OID")).getOrElse(0)
          val CODE: String = Option(rs.getString("CODE")).getOrElse("")
          val DESCRIPTION: String = Option(rs.getString("DESCRIPTION")).getOrElse("")
          items += Block(OID, CODE, DESCRIPTION)
        }
        rs.close()
        stmt.close()
        conn.close()
        items.toList.asJson.noSpaces
      case None =>
        items.toList.asJson.noSpaces
    }
  }
  def getHullPartListFromBsTree(project: String, docNum: String): Option[HullPL] = {
    val items = ListBuffer.empty[BsTreeItem]
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val query = Source.fromResource("queries/hullPartsFromBsTree.sql").mkString
        val s = c.prepareStatement(query)
        s.setString(0, docNum)
        val rs: ResultSet = s.executeQuery()
        while (rs.next()) {
          val PP_OID: Int = Option(rs.getInt("PP_OID")).getOrElse(0)
          val EXPL_OID: Int = Option(rs.getInt("EXPL_OID")).getOrElse(0)
          val BLOCK: String = Option(rs.getString("BLOCK")).getOrElse("")
          val CODE: String = Option(rs.getString("CODE")).getOrElse("")
          val PART_TYPE: Int = Option(rs.getInt("PART_TYPE")).getOrElse(0)
          val DESCRIPTION: String = Option(rs.getString("DESCRIPTION")).getOrElse("")
          val PARENT_NODE: Int = Option(rs.getInt("PARENT_NODE")).getOrElse(0)
          val ATOM_TYPE: Int = Option(rs.getInt("ATOM_TYPE")).getOrElse(0)
          val ORDINAL: Int = Option(rs.getInt("ORDINAL")).getOrElse(0)
          val TYPE_ATOM_TYPE: Int = Option(rs.getInt("TYPE_ATOM_TYPE")).getOrElse(0)
          val SYMBOL: String = Option(rs.getString("SYMBOL")).getOrElse("")
          val WEIGHT: Double = Option(rs.getDouble("WEIGHT")).getOrElse(0.0)
          val X_COG: Double = Option(rs.getDouble("X_COG")).getOrElse(0.0)
          val Y_COG: Double = Option(rs.getDouble("Y_COG")).getOrElse(0.0)
          val Z_COG: Double = Option(rs.getDouble("Z_COG")).getOrElse(0.0)

          val PRF_LENGTH: Double = Option(rs.getDouble("PRF_LENGTH")).getOrElse(0.0)
          val LINE_NAME: String = Option(rs.getString("LINE_NAME")).getOrElse("")
          val LINE_TYPE: String = Option(rs.getString("LINE_TYPE")).getOrElse("")
          val LINE_TYPE_DESCRIPTION: String = Option(rs.getString("LINE_TYPE_DESCRIPTION")).getOrElse("")
          val BS_ADN: String = Option(rs.getString("BS_ADN")).getOrElse("")
          val KSE: String = Option(rs.getString("KSE")).getOrElse("")
          val STOCKCODE: String = Option(rs.getString("STOCKCODE")).getOrElse("")
          items += BsTreeItem(
            PP_OID,
            EXPL_OID,
            BLOCK,
            CODE,
            PART_TYPE,
            DESCRIPTION,
            PARENT_NODE,
            ATOM_TYPE,
            ORDINAL,
            TYPE_ATOM_TYPE,
            SYMBOL,
            WEIGHT,
            X_COG,
            Y_COG,
            Z_COG,
            PRF_LENGTH,
            LINE_NAME,
            LINE_TYPE,
            LINE_TYPE_DESCRIPTION,
            BS_ADN,
            KSE,
            STOCKCODE)
        }
        rs.close()
        s.close()
        c.close()
        val hp = HullPL(new ObjectId(), project, items.toList, List.empty[Room], "", docNum, "", new Date().getTime, 0)
        Option(hp)
      case _ =>
        Option.empty[HullPL]
    }
  }
  def setHullPartListFromBsTree(project: String, docNum: String, user: String, revision: String): String ={
    getHullPartListFromBsTree(project, docNum) match {
      case Some(value) =>
        val mongo = mongoClient()

        val partLists = mongoDatabase().getCollection("partLists")
        val partListsHistory = mongoDatabase().getCollection("partListsHistory")

        Await.result(partLists.find[HullPL](new BasicDBObject("docNumber", docNum)).first().toFuture(), Duration(10, SECONDS)) match {
          case existSP: HullPL =>
            Await.result(partListsHistory.insertOne(Document.apply(existSP.asJson.noSpaces)).toFuture(), Duration(100, SECONDS))
          case _ => None
        }

//        value.setRevision(user, revision)
        Await.result(partLists.insertOne(Document.apply(value.asJson.noSpaces)).toFuture(), Duration(100, SECONDS))


        "success"
      case _ => "error"
    }
  }
}



