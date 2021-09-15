package local.hull

import com.mongodb.BasicDBObject
import io.circe.{Decoder, Encoder}
import local.common.DB
import local.common.DB.Room
import local.hull.BStree.{Block, BsTreeItem, HullPL}
import org.bson.types.ObjectId

import java.sql.{ResultSet, Statement}
import java.util.Date
import scala.collection.mutable.ListBuffer
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import org.mongodb.scala.{FindObservable, MongoCollection}
import org.mongodb.scala.result.InsertOneResult

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.impl.Promise
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global

object BStree {

  implicit val decodeObjectId: Decoder[ObjectId] = Decoder[String].map(str => new ObjectId(str))
  implicit val encodeObjectId: Encoder[ObjectId] = Encoder[String].contramap(_.toString)

  implicit val RoomDecoder: Decoder[Room] = deriveDecoder[Room]
  implicit val RoomEncoder: Encoder[Room] = deriveEncoder[Room]

  implicit val HullPLDecoder: Decoder[HullPL] = deriveDecoder[HullPL]
  implicit val HullPLEncoder: Encoder[HullPL] = deriveEncoder[HullPL]

  implicit val BlockDecoder: Decoder[Block] = deriveDecoder[Block]
  implicit val BlockEncoder: Encoder[Block] = deriveEncoder[Block]

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
                     name: String,
                     descr: String,
                     project: String,
                     rev: Int,
                     taskId: String,
                     user: String,
                     path: String,
                     date: Long,
                     content: List[BsTreeItem],
                     rooms: List[Room]
                   )

}


trait BStree extends DB {

  def genPartList(project: String, block: String, taskId: String, docNum: String, docName: String, user: String): String = {
    val mongo = configureMongoDB()
    val collectionMat: MongoCollection[HullPL] = mongo.mongoDatabase.getCollection("partlists")
    val sp: Option[HullPL] = {
      try {
        val existSP: HullPL = Await.result(collectionMat.find[HullPL](new BasicDBObject("taskId", taskId)).first().toFuture(), Duration(4, SECONDS))
        if (existSP != null) {
          Option[HullPL](existSP)
        } else {
          None
        }
      } catch {
        case _: Throwable => None
      }
    }
    sp match {
      case Some(existSP) => {
        existSP.asJson.noSpaces
      }
      case None => {
        val items = ListBuffer.empty[BsTreeItem]
        val sql = s"select\n        PP_OID,\n        EXPL_OID,\n        BLOCK,\n        CODE,\n        PART_TYPE,\n        DESCRIPTION,\n        PARENT_NODE,\n        ATOM_TYPE,\n        ORDINAL,\n        TYPE_ATOM_TYPE,\n        SYMBOL,\n        WEIGHT,\n        PRF_LENGTH,\n        LINE_NAME,\n        LINE_TYPE,\n        LINE_TYPE_DESCRIPTION,\n        BS_ADN,\n        GET_KSE_KPL(PP_OID,EXPL_OID, PART_TYPE, 0) as KSE,\n    (\n                select \n                PATT.THICKNESS*1000\n                from PRD_PART PP , INP_PLATE_ATT_DB IPLA, INP_PLATE_ATT_DB PATT , STD_PLATE SPL\n                     WHERE\n                     PP.oid= PP_OID AND\n                     PP.PART_TYPE IN (14,15,18,20,22,23,25,26)\n                     AND IPLA.INP_PART_OID = PP.OID\n                     AND PATT.INP_PART_OID=PP.OID\n                     AND (SPL.MATERIAL_OID=PATT.MATERIAL_OID AND SPL.THICKNESS=PATT.THICKNESS*1000)\n               \n                union all\n                select \n                PL.THICKNESS\n                from PRD_PART PP ,SHDK_PLATE PL, V_RPT_SHDK_PLATE_ATT ATT, STD_PLATE SPL\n                     WHERE  \n                      PP.oid= PP_OID AND  \n                      PP.PART_TYPE IN (9, 10)\n                      AND PL.PRD_PART_OID=PP.oid\n                      AND ATT.PRD_PART_OID=PP.oid\n                      AND (SPL.MATERIAL_OID=ATT.MATERIAL_OID AND SPL.THICKNESS=PL.THICKNESS)\n\n                union all\n                select \n                IPRA.PL_THICK_VALUE*1000\n                from PRD_PART PP, INP_LC_ATT_DB IPRA ,STD_PROFILE PROF, STD_PLATE SPL\n                     WHERE   \n                      PP.oid= PP_OID AND  \n                      PP.PART_TYPE IN (17)\n                      AND IPRA.INP_PART_OID = PP.OID\n                      AND PROF.OID=IPRA.STD_SECTION_OID\n                      AND (SPL.MATERIAL_OID=PROF.MATERIAL_OID AND SPL.THICKNESS=IPRA.PL_THICK_VALUE*1000)\n     ) \n     as thick, \n    (select \n                SPL.STORAGE_CODE\n                from PRD_PART PP , INP_PLATE_ATT_DB IPLA, INP_PLATE_ATT_DB PATT , STD_PLATE SPL\n                     WHERE\n                     PP.oid= PP_OID AND\n                     PP.PART_TYPE IN (14,15,18,20,22,23,25,26)\n                     AND IPLA.INP_PART_OID = PP.OID\n                     AND PATT.INP_PART_OID=PP.OID\n                     AND (SPL.MATERIAL_OID=PATT.MATERIAL_OID AND SPL.THICKNESS=PATT.THICKNESS*1000)\n                union all\n                select \n                STD.STOCK_CODE0\n                from PRD_PART PP , PRD_PROFILE PRF, STD_PROFILE STD\n                     WHERE \n                      PP.oid= PP_OID AND     \n                     PP.PART_TYPE IN (0,1,2,3,6)    \n                     AND PRF.PRD_PART_OID = PP.OID\n                     AND PRF.STD_PROFILE_OID=STD.OID\n                union all\n                select \n                SPL.STORAGE_CODE\n                from PRD_PART PP ,SHDK_PLATE PL, V_RPT_SHDK_PLATE_ATT ATT, STD_PLATE SPL\n                     WHERE  \n                      PP.oid= PP_OID AND  \n                      PP.PART_TYPE IN (9, 10)\n                      AND PL.PRD_PART_OID=PP.oid\n                      AND ATT.PRD_PART_OID=PP.oid\n                      AND (SPL.MATERIAL_OID=ATT.MATERIAL_OID AND SPL.THICKNESS=PL.THICKNESS)\n                      \n                union all\n                select \n                STD.STOCK_CODE0\n                from PRD_PART PP , INP_PROFILE_ATT_DB IPRA,STD_PROFILE STD\n                     WHERE \n                      PP.oid= PP_OID AND    \n                      PP.PART_TYPE IN (16,19,21,24)\n                      AND IPRA.INP_PART_OID = PP.OID\n                      AND IPRA.STD_PROFILE_OID=STD.OID\n                union all\n                select \n                SPL.STORAGE_CODE\n                from PRD_PART PP, INP_LC_ATT_DB IPRA ,STD_PROFILE PROF, STD_PLATE SPL\n                     WHERE   \n                      PP.oid= PP_OID AND  \n                      PP.PART_TYPE IN (17)\n                      AND IPRA.INP_PART_OID = PP.OID\n                      AND PROF.OID=IPRA.STD_SECTION_OID\n                      AND (SPL.MATERIAL_OID=PROF.MATERIAL_OID AND SPL.THICKNESS=IPRA.PL_THICK_VALUE*1000)\n    ) \n    as STOCKCODE\nfrom (\nselect   \n        PP.OID as PP_OID,\n        PEP.OID as EXPL_OID,\n        get_block_code(PP.BLOCK_OID) as BLOCK,\n        PP.CODE,\n        PP.PART_TYPE,\n        BSN.DESCRIPTION,\n        BSN.PARENT_NODE,\n        BSN.ATOM_TYPE,\n        BNT.ORDINAL,\n        BNT.ATOM_TYPE as TYPE_ATOM_TYPE,\n        BNT.SYMBOL,\n        PEP.WEIGHT,\n        RPT_GET_PART_LENGTH(PP.OID)*1000 as PRF_LENGTH,\n        (select NAME from BS_NODE where OID=BSN.PARENT_NODE) as LINE_NAME,\n        (select NAME from BS_NODE_TYPE where OID= (select BS_NODE_TYPE_OID from BS_NODE where OID=BSN.PARENT_NODE) ) as LINE_TYPE,\n        (select DESCRIPTION from BS_NODE_TYPE where OID= (select BS_NODE_TYPE_OID from BS_NODE where OID=BSN.PARENT_NODE) ) as LINE_TYPE_DESCRIPTION,\n        bs_node_adn_from_oid(BSN.OID) BS_ADN\n        from BS_NODE BSN ,BS_NODE_TYPE BNT,BS_ATOM_FIXED_ATTRIBUTE  AFA,BS_DESIGN_ATOM BDA, BS_DESIGN_NODE BDN, PRD_EXPL_PART PEP ,PRD_PART PP\n        where \n        get_block_code(PP.BLOCK_OID)='${block}'\n        AND BSN.BS_NODE_TYPE_OID=BNT.OID\n        AND BSN.OID = AFA.BS_NODE_OID\n        AND AFA.BS_DS_ATOM_OID = BDA.OID\n        AND BDA.BS_DESIGN_NODE_OID = BDN.OID\n        AND PEP.OID=BDN.MODEL_OID\n        AND PEP.PRD_PART_OID=PP.OID\n        AND BSN.ATOM_TYPE=3\n        )"
        oracleConnection() match {
          case Some(conn) => {
            try {
              val stmt: Statement = conn.createStatement()
              val rs: ResultSet = stmt.executeQuery(sql)
              while (rs.next()) {
                val PP_OID: Int = Option[Int](rs.getInt("PP_OID")).getOrElse(0)
                val EXPL_OID: Int = Option[Int](rs.getInt("EXPL_OID")).getOrElse(0)
                val BLOCK: String = Option[String](rs.getString("BLOCK")).getOrElse("")
                val CODE: String = Option[String](rs.getString("CODE")).getOrElse("")
                val PART_TYPE: Int = Option[Int](rs.getInt("PART_TYPE")).getOrElse(0)
                val DESCRIPTION: String = Option[String](rs.getString("DESCRIPTION")).getOrElse("")
                val PARENT_NODE: Int = Option[Int](rs.getInt("PARENT_NODE")).getOrElse(0)
                val ATOM_TYPE: Int = Option[Int](rs.getInt("ATOM_TYPE")).getOrElse(0)
                val ORDINAL: Int = Option[Int](rs.getInt("ORDINAL")).getOrElse(0)
                val TYPE_ATOM_TYPE: Int = Option[Int](rs.getInt("TYPE_ATOM_TYPE")).getOrElse(0)
                val SYMBOL: String = Option[String](rs.getString("SYMBOL")).getOrElse("")
                val WEIGHT: Double = Option[Double](rs.getDouble("WEIGHT")).getOrElse(0.0)
                val PRF_LENGTH: Double = Option[Double](rs.getDouble("PRF_LENGTH")).getOrElse(0.0)
                val LINE_NAME: String = Option[String](rs.getString("LINE_NAME")).getOrElse("")
                val LINE_TYPE: String = Option[String](rs.getString("LINE_TYPE")).getOrElse("")
                val LINE_TYPE_DESCRIPTION: String = Option[String](rs.getString("LINE_TYPE_DESCRIPTION")).getOrElse("")
                val BS_ADN: String = Option[String](rs.getString("BS_ADN")).getOrElse("")
                val KSE: String = Option[String](rs.getString("KSE")).getOrElse("")
                val STOCKCODE: String = Option[String](rs.getString("STOCKCODE")).getOrElse("")
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
                  PRF_LENGTH,
                  LINE_NAME,
                  LINE_TYPE,
                  LINE_TYPE_DESCRIPTION,
                  BS_ADN,
                  KSE,
                  STOCKCODE)
              }
              conn.close()
            }
            catch {
              case _: Throwable => ""
            }
          }
          case None => ""
        }
        val hp = HullPL(new ObjectId(), docNum, docName, project, 0, taskId, user, "", new Date().getTime, items.toList, List[Room]())
        Await.result(collectionMat.insertOne(hp).toFuture(), Duration(100, SECONDS))
        hp.asJson.noSpaces
      }
    }

  }

  def genBlocks(project: String): String = {
    val items = ListBuffer.empty[Block]
    oracleConnection(project) match {
      case Some(conn) => {
        try {
          val sql = "SELECT OID,CODE,DESCRIPTION FROM BLOCK order by code"
          val stmt: Statement = conn.createStatement()
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            val OID: Int = Option[Int](rs.getInt("OID")).getOrElse(0)
            val CODE: String = Option[String](rs.getString("CODE")).getOrElse("")
            val DESCRIPTION: String = Option[String](rs.getString("DESCRIPTION")).getOrElse("")
            items += Block(OID, CODE, DESCRIPTION)
          }
          conn.close()
          items.toList.asJson.noSpaces
        }
        catch {
          case x: Throwable =>{
            println(x.toString)
            items.toList.asJson.noSpaces
          }
        }
      }
      case None => {
        items.toList.asJson.noSpaces
      }
    }
  }
}



