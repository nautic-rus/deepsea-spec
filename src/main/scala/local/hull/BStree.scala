package local.hull

import com.mongodb.BasicDBObject
import deepsea.App
import deepsea.actors.ActorManager
import deepsea.database.DatabaseManager.GetOracleConnection
import deepsea.hull.HullManager.{GetHullParts, HullPart}
import io.circe.{Decoder, Encoder}
import local.hull.BStree.{Block, BsTreeItem, HullPL, Room}
import org.bson.types.ObjectId
import org.mongodb.scala.bson.codecs.Macros._

import java.sql.{ResultSet, Statement}
import java.util.Date
import scala.collection.mutable.ListBuffer
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import local.sql.ConnectionManager.{connectionByProject, mongoClient}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{Document, FindObservable, MongoCollection}
import play.api.libs.json.Json

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source


object BStree {
  case class Room(num:String,name:String)

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
    def setRevision(user: String, revision: String, version: Int): Unit ={
      this.user = user
      this.revision = revision
      this.version = version
      date = new Date().getTime
    }
  }

}


trait BStree  {

  def codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[HullPL],
    classOf[BsTreeItem],
    classOf[Block],
  ), DEFAULT_CODEC_REGISTRY)

//  def genPartList(project: String, block: String, taskId: String, docNum: String, docName: String, user: String): String = {
//    val mongo = mongoClient()
//
//    val collectionMat: MongoCollection[HullPL] = mongo.getDatabase(App.conf.getString("mongo.db")).withCodecRegistry(codecRegistry).getCollection("partlists")
//
//    val sp: Option[HullPL] = {
//      try {
//        val existSP: HullPL = Await.result(collectionMat.find[HullPL](new BasicDBObject("taskId", taskId)).first().toFuture(), Duration(4, SECONDS))
//        if (existSP != null) {
//          Option[HullPL](existSP)
//        } else {
//          None
//        }
//      } catch {
//        case _: Throwable => None
//      }
//    }
//
//    sp match {
//      case Some(existSP) => {
//        existSP.asJson.noSpaces
//      }
//      case None => {
//        val items = ListBuffer.empty[BsTreeItem]
//        val sql = s"select\n        PP_OID,\n        EXPL_OID,\n        BLOCK,\n        CODE,\n        PART_TYPE,\n        DESCRIPTION,\n        PARENT_NODE,\n        ATOM_TYPE,\n        ORDINAL,\n        TYPE_ATOM_TYPE,\n        SYMBOL,\n        WEIGHT,\n        " +
//          s"X_COG,Y_COG,Z_COG,  PRF_LENGTH,\n        LINE_NAME,\n        LINE_TYPE,\n        LINE_TYPE_DESCRIPTION,\n        BS_ADN,\n        GET_KSE_KPL(PP_OID,EXPL_OID, PART_TYPE, 0) as KSE,\n    (\n                select \n                PATT.THICKNESS*1000\n                from PRD_PART PP , INP_PLATE_ATT_DB IPLA, INP_PLATE_ATT_DB PATT , STD_PLATE SPL\n                     WHERE\n                     PP.oid= PP_OID AND\n                     PP.PART_TYPE IN (14,15,18,20,22,23,25,26)\n                     AND IPLA.INP_PART_OID = PP.OID\n                     AND PATT.INP_PART_OID=PP.OID\n                     AND (SPL.MATERIAL_OID=PATT.MATERIAL_OID AND SPL.THICKNESS=PATT.THICKNESS*1000)\n               \n                union all\n                select \n                PL.THICKNESS\n                from PRD_PART PP ,SHDK_PLATE PL, V_RPT_SHDK_PLATE_ATT ATT, STD_PLATE SPL\n                     WHERE  \n                      PP.oid= PP_OID AND  \n                      PP.PART_TYPE IN (9, 10)\n                      AND PL.PRD_PART_OID=PP.oid\n                      AND ATT.PRD_PART_OID=PP.oid\n                      AND (SPL.MATERIAL_OID=ATT.MATERIAL_OID AND SPL.THICKNESS=PL.THICKNESS)\n\n                union all\n                select \n                IPRA.PL_THICK_VALUE*1000\n                from PRD_PART PP, INP_LC_ATT_DB IPRA ,STD_PROFILE PROF, STD_PLATE SPL\n                     WHERE   \n                      PP.oid= PP_OID AND  \n                      PP.PART_TYPE IN (17)\n                      AND IPRA.INP_PART_OID = PP.OID\n                      AND PROF.OID=IPRA.STD_SECTION_OID\n                      AND (SPL.MATERIAL_OID=PROF.MATERIAL_OID AND SPL.THICKNESS=IPRA.PL_THICK_VALUE*1000)\n     ) \n     as thick, \n    (select \n                SPL.STORAGE_CODE\n                from PRD_PART PP , INP_PLATE_ATT_DB IPLA, INP_PLATE_ATT_DB PATT , STD_PLATE SPL\n                     WHERE\n                     PP.oid= PP_OID AND\n                     PP.PART_TYPE IN (14,15,18,20,22,23,25,26)\n                     AND IPLA.INP_PART_OID = PP.OID\n                     AND PATT.INP_PART_OID=PP.OID\n                     AND (SPL.MATERIAL_OID=PATT.MATERIAL_OID AND SPL.THICKNESS=PATT.THICKNESS*1000)\n                union all\n                select \n                STD.STOCK_CODE0\n                from PRD_PART PP , PRD_PROFILE PRF, STD_PROFILE STD\n                     WHERE \n                      PP.oid= PP_OID AND     \n                     PP.PART_TYPE IN (0,1,2,3,6)    \n                     AND PRF.PRD_PART_OID = PP.OID\n                     AND PRF.STD_PROFILE_OID=STD.OID\n                union all\n                select \n                SPL.STORAGE_CODE\n                from PRD_PART PP ,SHDK_PLATE PL, V_RPT_SHDK_PLATE_ATT ATT, STD_PLATE SPL\n                     WHERE  \n                      PP.oid= PP_OID AND  \n                      PP.PART_TYPE IN (9, 10)\n                      AND PL.PRD_PART_OID=PP.oid\n                      AND ATT.PRD_PART_OID=PP.oid\n                      AND (SPL.MATERIAL_OID=ATT.MATERIAL_OID AND SPL.THICKNESS=PL.THICKNESS)\n                      \n                union all\n                select \n                STD.STOCK_CODE0\n                from PRD_PART PP , INP_PROFILE_ATT_DB IPRA,STD_PROFILE STD\n                     WHERE \n                      PP.oid= PP_OID AND    \n                      PP.PART_TYPE IN (16,19,21,24)\n                      AND IPRA.INP_PART_OID = PP.OID\n                      AND IPRA.STD_PROFILE_OID=STD.OID\n                union all\n                select \n                SPL.STORAGE_CODE\n                from PRD_PART PP, INP_LC_ATT_DB IPRA ,STD_PROFILE PROF, STD_PLATE SPL\n                     WHERE   \n                      PP.oid= PP_OID AND  \n                      PP.PART_TYPE IN (17)\n                      AND IPRA.INP_PART_OID = PP.OID\n                      AND PROF.OID=IPRA.STD_SECTION_OID\n                      AND (SPL.MATERIAL_OID=PROF.MATERIAL_OID AND SPL.THICKNESS=IPRA.PL_THICK_VALUE*1000)\n    ) \n    as STOCKCODE\nfrom (\nselect   \n        PP.OID as PP_OID,\n        PEP.OID as EXPL_OID,\n        get_block_code(PP.BLOCK_OID) as BLOCK,\n        PP.CODE,\n        PP.PART_TYPE,\n        BSN.DESCRIPTION,\n        BSN.PARENT_NODE,\n        BSN.ATOM_TYPE,\n        BNT.ORDINAL,\n        BNT.ATOM_TYPE as TYPE_ATOM_TYPE,\n        BNT.SYMBOL,\n        " +
//          s"PEP.WEIGHT,PEP.X_COG,PEP.Y_COG,PEP.Z_COG,        RPT_GET_PART_LENGTH(PP.OID)*1000 as PRF_LENGTH,\n        (select NAME from BS_NODE where OID=BSN.PARENT_NODE) as LINE_NAME,\n        (select NAME from BS_NODE_TYPE where OID= (select BS_NODE_TYPE_OID from BS_NODE where OID=BSN.PARENT_NODE) ) as LINE_TYPE,\n        (select DESCRIPTION from BS_NODE_TYPE where OID= (select BS_NODE_TYPE_OID from BS_NODE where OID=BSN.PARENT_NODE) ) as LINE_TYPE_DESCRIPTION,\n        bs_node_adn_from_oid(BSN.OID) BS_ADN\n        from BS_NODE BSN ,BS_NODE_TYPE BNT,BS_ATOM_FIXED_ATTRIBUTE  AFA,BS_DESIGN_ATOM BDA, BS_DESIGN_NODE BDN, PRD_EXPL_PART PEP ,PRD_PART PP\n        where \n        get_block_code(PP.BLOCK_OID)='${block}'\n        AND BSN.BS_NODE_TYPE_OID=BNT.OID\n        AND BSN.OID = AFA.BS_NODE_OID\n        AND AFA.BS_DS_ATOM_OID = BDA.OID\n        AND BDA.BS_DESIGN_NODE_OID = BDN.OID\n        AND PEP.OID=BDN.MODEL_OID\n        AND PEP.PRD_PART_OID=PP.OID\n        AND BSN.ATOM_TYPE=3\n        )"
//        /*
//                val sqlNew="select        \nPP_OID,        \nEXPL_OID,        \nBLOCK,        \nCODE,        \nPART_TYPE,        \nDESCRIPTION,        \nPARENT_NODE,        \nATOM_TYPE,        \nORDINAL,        \nTYPE_ATOM_TYPE,        \nSYMBOL,        \nWEIGHT,\nX_COG,\nY_COG,\nZ_COG,        \nPRF_LENGTH,        \nLINE_NAME,        \nLINE_TYPE,        \nLINE_TYPE_DESCRIPTION,        \nBS_ADN,        \nGET_KSE_KPL(PP_OID,EXPL_OID, PART_TYPE, 0) as KSE,    \n\t(\n\t\tselect     PATT.THICKNESS*1000   from PRD_PART PP , INP_PLATE_ATT_DB IPLA, INP_PLATE_ATT_DB PATT , STD_PLATE SPL  WHERE     \n\t\tPP.oid= PP_OID \n\t\tAND PP.PART_TYPE IN (14,15,18,20,22,23,25,26) \n\t\tAND IPLA.INP_PART_OID = PP.OID                     \n\t\tAND PATT.INP_PART_OID=PP.OID                     \n\t\tAND (SPL.MATERIAL_OID=PATT.MATERIAL_OID \n\t\tAND SPL.THICKNESS=PATT.THICKNESS*1000)                               \n\t\tunion all                \n\t\tselect     PL.THICKNESS  from PRD_PART PP ,SHDK_PLATE PL, V_RPT_SHDK_PLATE_ATT ATT, STD_PLATE SPL   WHERE                        \n\t\tPP.oid= PP_OID \n\t\tAND   PP.PART_TYPE IN (9, 10)                      \n\t\tAND PL.PRD_PART_OID=PP.oid                      \n\t\tAND ATT.PRD_PART_OID=PP.oid                      \n\t\tAND (SPL.MATERIAL_OID=ATT.MATERIAL_OID \n\t\tAND SPL.THICKNESS=PL.THICKNESS)                \n\t\tunion all                \n\t\tselect    IPRA.PL_THICK_VALUE*1000   from PRD_PART PP, INP_LC_ATT_DB IPRA ,STD_PROFILE PROF, STD_PLATE SPL  WHERE                         \n\t\tPP.oid= PP_OID \n\t\tAND  PP.PART_TYPE IN (17)                      \n\t\tAND IPRA.INP_PART_OID = PP.OID                      \n\t\tAND PROF.OID=IPRA.STD_SECTION_OID                      \n\t\tAND (SPL.MATERIAL_OID=PROF.MATERIAL_OID \n\t\tAND SPL.THICKNESS=IPRA.PL_THICK_VALUE*1000) \n\t)      as thick,     \n\t(\n\t\tselect  SPL.STORAGE_CODE   from PRD_PART PP , INP_PLATE_ATT_DB IPLA, INP_PLATE_ATT_DB PATT , STD_PLATE SPL   WHERE    \n\t\tPP.oid= PP_OID \n\t\tAND   PP.PART_TYPE IN (14,15,18,20,22,23,25,26)  \n\t\tAND IPLA.INP_PART_OID = PP.OID    \n\t\tAND PATT.INP_PART_OID=PP.OID  \n\t\tAND (SPL.MATERIAL_OID=PATT.MATERIAL_OID AND SPL.THICKNESS=PATT.THICKNESS*1000)                \n\t\tunion all                \n\t\tselect  STD.STOCK_CODE0                from PRD_PART PP , PRD_PROFILE PRF, STD_PROFILE STD  WHERE                       \n\t\tPP.oid= PP_OID \n\t\tAND PP.PART_TYPE IN (0,1,2,3,6)                         \n\t\tAND PRF.PRD_PART_OID = PP.OID                     \n\t\tAND PRF.STD_PROFILE_OID=STD.OID                \n\t\tunion all                \n\t\tselect SPL.STORAGE_CODE  from PRD_PART PP ,SHDK_PLATE PL, V_RPT_SHDK_PLATE_ATT ATT, STD_PLATE SPL  WHERE                        \n\t\tPP.oid= PP_OID \n\t\tAND  PP.PART_TYPE IN (9, 10)                      \n\t\tAND PL.PRD_PART_OID=PP.oid                      \n\t\tAND ATT.PRD_PART_OID=PP.oid                      \n\t\tAND (SPL.MATERIAL_OID=ATT.MATERIAL_OID AND SPL.THICKNESS=PL.THICKNESS)                                      \n\t\tunion all                \n\t\tselect   STD.STOCK_CODE0      from PRD_PART PP , INP_PROFILE_ATT_DB IPRA,STD_PROFILE STD WHERE                       \n\t\tPP.oid= PP_OID \n\t\tAND  PP.PART_TYPE IN (16,19,21,24)                      \n\t\tAND IPRA.INP_PART_OID = PP.OID                      \n\t\tAND IPRA.STD_PROFILE_OID=STD.OID                \n\t\tunion all                \n\t\tselect   SPL.STORAGE_CODE from PRD_PART PP, INP_LC_ATT_DB IPRA ,STD_PROFILE PROF, STD_PLATE SPL  WHERE                         \n\t\tPP.oid= PP_OID \n\t\tAND  PP.PART_TYPE IN (17)                      \n\t\tAND IPRA.INP_PART_OID = PP.OID                      \n\t\tAND PROF.OID=IPRA.STD_SECTION_OID                      \n\t\tAND (SPL.MATERIAL_OID=PROF.MATERIAL_OID \n\t\tAND SPL.THICKNESS=IPRA.PL_THICK_VALUE*1000)    \n\t)     as STOCKCODE\n\nfrom \n\t(\n\t\tselect  \n\t\tPP.OID as PP_OID,        \n\t\tPEP.OID as EXPL_OID,        \n\t\tget_block_code(PP.BLOCK_OID) as BLOCK,        \n\t\tPP.CODE,        \n\t\tPP.PART_TYPE,        \n\t\tBSN.DESCRIPTION,        \n\t\tBSN.PARENT_NODE,        \n\t\tBSN.ATOM_TYPE,        \n\t\tBNT.ORDINAL,        \n\t\tBNT.ATOM_TYPE as TYPE_ATOM_TYPE,        \n\t\tBNT.SYMBOL,        \n\t\tPEP.WEIGHT,   \n\t\tPEP.X_COG,\n\t\tPEP.Y_COG,\n\t\tPEP.Z_COG,     \n\t\tRPT_GET_PART_LENGTH(PP.OID)*1000 as PRF_LENGTH,        \n\t\t(select NAME from BS_NODE where OID=BSN.PARENT_NODE) as LINE_NAME,        \n\t\t(select NAME from BS_NODE_TYPE where OID= (select BS_NODE_TYPE_OID from BS_NODE where OID=BSN.PARENT_NODE) ) as LINE_TYPE,        \n\t\t(select DESCRIPTION from BS_NODE_TYPE where OID= (select BS_NODE_TYPE_OID from BS_NODE where OID=BSN.PARENT_NODE) ) as LINE_TYPE_DESCRIPTION,        \n\t\tbs_node_adn_from_oid(BSN.OID) BS_ADN        \n\t\tfrom BS_NODE BSN ,BS_NODE_TYPE BNT,BS_ATOM_FIXED_ATTRIBUTE  AFA,BS_DESIGN_ATOM BDA, BS_DESIGN_NODE BDN, PRD_EXPL_PART PEP ,PRD_PART PP        \n\t\twhere\n\t\t  bs_node_adn_from_oid(BSN.OID) " +
//                  "LIKE '%NR004-150-101%' AND  \n\t\t BSN.BS_NODE_TYPE_OID=BNT.OID       \n\t\tAND BSN.OID = AFA.BS_NODE_OID        \n\t\tAND AFA.BS_DS_ATOM_OID = BDA.OID        \n\t\tAND BDA.BS_DESIGN_NODE_OID = BDN.OID        \n\t\tAND PEP.OID=BDN.MODEL_OID        \n\t\tAND PEP.PRD_PART_OID=PP.OID        \n\t\tAND BSN.ATOM_TYPE=3 \n\t)\n\t"
//        */
//
//        connectionByProject(project) match {
//          case Some(conn) => {
//            try {
//              val stmt: Statement = conn.createStatement()
//              val rs: ResultSet = stmt.executeQuery(sql)
//              while (rs.next()) {
//                val PP_OID: Int = Option[Int](rs.getInt("PP_OID")).getOrElse(0)
//                val EXPL_OID: Int = Option[Int](rs.getInt("EXPL_OID")).getOrElse(0)
//                val BLOCK: String = Option[String](rs.getString("BLOCK")).getOrElse("")
//                val CODE: String = Option[String](rs.getString("CODE")).getOrElse("")
//                val PART_TYPE: Int = Option[Int](rs.getInt("PART_TYPE")).getOrElse(0)
//                val DESCRIPTION: String = Option[String](rs.getString("DESCRIPTION")).getOrElse("")
//                val PARENT_NODE: Int = Option[Int](rs.getInt("PARENT_NODE")).getOrElse(0)
//                val ATOM_TYPE: Int = Option[Int](rs.getInt("ATOM_TYPE")).getOrElse(0)
//                val ORDINAL: Int = Option[Int](rs.getInt("ORDINAL")).getOrElse(0)
//                val TYPE_ATOM_TYPE: Int = Option[Int](rs.getInt("TYPE_ATOM_TYPE")).getOrElse(0)
//                val SYMBOL: String = Option[String](rs.getString("SYMBOL")).getOrElse("")
//                val WEIGHT: Double = Option[Double](rs.getDouble("WEIGHT")).getOrElse(0.0)
//                val X_COG: Double = Option[Double](rs.getDouble("X_COG")).getOrElse(0.0)
//                val Y_COG: Double = Option[Double](rs.getDouble("Y_COG")).getOrElse(0.0)
//                val Z_COG: Double = Option[Double](rs.getDouble("Z_COG")).getOrElse(0.0)
//
//                val PRF_LENGTH: Double = Option[Double](rs.getDouble("PRF_LENGTH")).getOrElse(0.0)
//                val LINE_NAME: String = Option[String](rs.getString("LINE_NAME")).getOrElse("")
//                val LINE_TYPE: String = Option[String](rs.getString("LINE_TYPE")).getOrElse("")
//                val LINE_TYPE_DESCRIPTION: String = Option[String](rs.getString("LINE_TYPE_DESCRIPTION")).getOrElse("")
//                val BS_ADN: String = Option[String](rs.getString("BS_ADN")).getOrElse("")
//                val KSE: String = Option[String](rs.getString("KSE")).getOrElse("")
//                val STOCKCODE: String = Option[String](rs.getString("STOCKCODE")).getOrElse("")
//                items += BsTreeItem(
//                  PP_OID,
//                  EXPL_OID,
//                  BLOCK,
//                  CODE,
//                  PART_TYPE,
//                  DESCRIPTION,
//                  PARENT_NODE,
//                  ATOM_TYPE,
//                  ORDINAL,
//                  TYPE_ATOM_TYPE,
//                  SYMBOL,
//                  WEIGHT,
//                  X_COG,
//                  Y_COG,
//                  Z_COG,
//                  PRF_LENGTH,
//                  LINE_NAME,
//                  LINE_TYPE,
//                  LINE_TYPE_DESCRIPTION,
//                  BS_ADN,
//                  KSE,
//                  STOCKCODE)
//              }
//              conn.close()
//            }
//            catch {
//              case _: Throwable => ""
//            }
//          }
//          case None => ""
//        }
//        val hp = HullPL(new ObjectId(), docNum, docName, project, "0", taskId, user, "", new Date().getTime, items.toList, List[Room]())
//        Await.result(collectionMat.insertOne(hp).toFuture(), Duration(100, SECONDS))
//        hp.asJson.noSpaces
//      }
//    }
//
//  }



  def genBlocks(project: String): String = {
    val items = ListBuffer.empty[Block]
    GetOracleConnection(project) match {
      case Some(conn) =>
        val sql = "SELECT OID,CODE,DESCRIPTION FROM BLOCK order by code"
        val stmt: Statement = conn.createStatement()
        val rs: ResultSet = stmt.executeQuery(sql)
        while (rs.next()) {
          val OID: Int = Option[Int](rs.getInt("OID")).getOrElse(0)
          val CODE: String = Option[String](rs.getString("CODE")).getOrElse("")
          val DESCRIPTION: String = Option[String](rs.getString("DESCRIPTION")).getOrElse("")
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
    GetOracleConnection(project) match {
      case Some(c) =>
        val query = Source.fromResource("queries/hullPartsFromBsTree.sql").mkString
        val s = c.prepareStatement(query)
        s.setString(0, docNum)
        val rs: ResultSet = s.executeQuery()
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
          val X_COG: Double = Option[Double](rs.getDouble("X_COG")).getOrElse(0.0)
          val Y_COG: Double = Option[Double](rs.getDouble("Y_COG")).getOrElse(0.0)
          val Z_COG: Double = Option[Double](rs.getDouble("Z_COG")).getOrElse(0.0)

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
        val hp = HullPL(new ObjectId(), project, items.toList, List.empty[Room], "", docNum, "", new Date().getTime)
        Option(hp)
      case _ =>
        Option.empty[HullPL]
    }
  }
  def setHullPartListFromBsTree(project: String, docNum: String, user: String, revision: String): String ={
    getHullPartListFromBsTree(project, docNum) match {
      case Some(value) =>
        val mongo = mongoClient()

        val partLists = mongo.getDatabase(App.conf.getString("mongo.db")).withCodecRegistry(codecRegistry).getCollection("partLists")
        val partListsHistory = mongo.getDatabase(App.conf.getString("mongo.db")).withCodecRegistry(codecRegistry).getCollection("partListsHistory")

        Await.result(partLists.find[HullPL](new BasicDBObject("docNumber", docNum)).first().toFuture(), Duration(10, SECONDS)) match {
          case existSP: HullPL =>
            Await.result(partListsHistory.insertOne(Document.apply(existSP.asJson.noSpaces)).toFuture(), Duration(100, SECONDS))
          case _ => None
        }

        value.setRevision(user, revision)
        Await.result(partLists.insertOne(Document.apply(value.asJson.noSpaces)).toFuture(), Duration(100, SECONDS))


        "success"
      case _ => "error"
    }
  }
}



