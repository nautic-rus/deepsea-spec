package local.eqmount

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps


import local.common.Codecs
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import java.util.Date
import scala.collection.mutable.ListBuffer

object EqFoundationManager extends Codecs {

  case class EqFoundation(
                           BSOID: Int,
                           BSNAME: String,
                           BSDESCRIPTION: String,
                           BSMOUNTING_SEQ_ORDER: Int,
                           BSADN: String,
                           BSFOUNDATION: String,
                           BSFOUNDATION_DESCR: String,
                           EOID: Int,
                           ETYPE: Int,
                           EUSERID: String,
                           EVERSION: Int,
                           EUSER_NEW: String,
                           EDATE_NEW: Long,
                           EUSER_MOD: String,
                           EDATE_MOD: Long,
                           EUUID: String,
                           COID: Int,
                           CTYPE: Int,
                           CABBREV: String,
                           CELEM_CLASS: Int,
                           CWEIGHT: Double,
                           CSUPLID: Int,
                           CAC: Int,
                           CMT: String,
                           CSTOCK_CODE: String,
                           XPOS: Double,
                           YPOS: Double,
                           ZPOS: Double,
                           XCOG: Double,
                           YCOG: Double,
                           ZCOG: Double,
                           ZONENAME: String,
                           ZONEDESCR: String,
                           SYSTEMNAME: String,
                           SYSTEMDESCR: String,
                           HULLBLOCK: String = "",
                           eqStatus: Int = 0,
                           eqStatusUser: String = "",
                           eqStatusDate: Long = 0,
                           foundationStatus: Int = 0
                         )


  private def foundationEqSql(): String = "select \n  B.OID as BSOID,\n  B.NAME as BSNAME,\n  B.DESCRIPTION AS BSDESCRIPTION,\n  B.MOUNTING_SEQ_ORDER AS BSMOUNTING_SEQ_ORDER, \n  B.ADN AS BSADN,\n  (select NAME from BS_NODE where OID=B.PARENT_NODE)  AS BSFOUNDATION,\n  (select DESCRIPTION from BS_NODE where OID=B.PARENT_NODE)  AS BSFOUNDATION_DESCR,\n  E.OID as EOID,\n  E.TYPE as ETYPE,\n  E.USERID as EUSERID,\n  E.VERSION as EVERSION,\n  E.USER_NEW as EUSER_NEW,\n  E.DATE_NEW as EDATE_NEW,\n  E.USER_MOD as EUSER_MOD,\n  E.DATE_MOD as EDATE_MOD,\n  E.UUID as EUUID,\n  C.OID as COID,\n  C.TYPE as CTYPE,\n  C.ABBREV  as CABBREV,\n  C.ELEM_CLASS as CELEM_CLASS,\n  C.WEIGHT as CWEIGHT,\n  C.SUPLID as CSUPLID,\n  C.AC as CAC,\n  C.MT as CMT,\n  C.STOCK_CODE as CSTOCK_CODE,\n ELEMPOS.XPOS(E.OID,E.TYPE,E.DECK) as XPOS,\n ELEMPOS.YPOS(E.OID,E.TYPE,E.DECK) as YPOS,\n ELEMPOS.ZPOS(E.OID,E.TYPE,E.DECK) as ZPOS,\n ELEMPOS.XCOG(E.OID,E.TYPE,E.DECK,E.COMP) as XCOG,\n ELEMPOS.YCOG(E.OID,E.TYPE,E.DECK,E.COMP) as YCOG,\n ELEMPOS.ZCOG(E.OID,E.TYPE,E.DECK,E.COMP) as ZCOG, \n Z.NAME as ZONENAME,\n ZL.DESCR as ZONEDESCR,\n S.NAME as SYSTEMNAME,\n SL.DESCR as SYSTEMDESCR\n\n  from\n  (\n          select\n          OID,\n          NAME,\n          DESCRIPTION,\n          PARENT_NODE,\n          STD_LIBRARY,\n          STD_NAME,\n          ATOM_TYPE,\n          BS_NODE_TYPE_OID,\n          MOUNTING_SEQ_ORDER, \n          bs_node_adn_from_oid(OID) as ADN\n          from BS_NODE\n  ) \n  B, V_BS_NODE_DS_NODE DS, ELEMENT E, COMPONENT C, SYSTEMS S, SYSTEMS_LANG SL,\n  ZONE Z,ZONE_LANG ZL\n  where B.ADN like '%/Foundation/%' AND\n  B.ATOM_TYPE=3 AND\n  B.OID=NODE_OID AND\n  E.OID=DS.MODEL_OID AND\n  C.OID=E.COMP and\n  Z.OID=E.ZONE and\n  ZL.ZONE=Z.OID AND\n  ZL.LANG=-2 AND\n  S.OID=E.SYSTEM and\n  SL.SYSTEM=S.OID AND\n  SL.LANG=-2\n  "

  private def updateStatus(bsOid: Int, status: String) = s"update BS_NODE set description='${status}' where OID = ${bsOid.toString}"

  private def blocksSql(in: String) = s"select code, description from block where description in (${in})"

  def foranFoudationsAndEqs(project: String): List[EqFoundation] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = foundationEqSql()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[EqFoundation]
          while (rs.next()) {
            val bsDescr: String = Option(rs.getString("BSDESCRIPTION")).getOrElse("")
            case class StatuseTmp(eqStatus: Int = 0, eqStatusUser: String = "", eqStatusDate: Long = 0)

            val status: StatuseTmp = {
              if (bsDescr.contains("@")) {
                val arrData: Array[String] = bsDescr.split("@")
                if (arrData.size == 3) {
                  val user: String = arrData(1)
                  val eqDate: Long = arrData(2).toLongOption.getOrElse(0)
                  if (user.nonEmpty && eqDate > 0) {
                    StatuseTmp(1, user, eqDate)
                  } else {
                    StatuseTmp()
                  }

                } else {
                  StatuseTmp()
                }
              } else {
                StatuseTmp()
              }
            }


            ret += EqFoundation(
              Option(rs.getInt("BSOID")).getOrElse(0),
              Option(rs.getString("BSNAME")).getOrElse(""),
              bsDescr,
              Option(rs.getInt("BSMOUNTING_SEQ_ORDER")).getOrElse(0),
              Option(rs.getString("BSADN")).getOrElse(""),
              Option(rs.getString("BSFOUNDATION")).getOrElse(""),
              Option(rs.getString("BSFOUNDATION_DESCR")).getOrElse(""),
              Option(rs.getInt("EOID")).getOrElse(0),
              Option(rs.getInt("ETYPE")).getOrElse(0),
              Option(rs.getString("EUSERID")).getOrElse(""),
              Option(rs.getInt("EVERSION")).getOrElse(0),
              Option(rs.getString("EUSER_NEW")).getOrElse(""),
              Option(rs.getDate("EDATE_NEW")).getOrElse(new Date()).getTime,
              Option(rs.getString("EUSER_MOD")).getOrElse(""),
              Option(rs.getDate("EDATE_MOD")).getOrElse(new Date()).getTime,
              Option(rs.getString("EUUID")).getOrElse(""),
              Option(rs.getInt("COID")).getOrElse(0),
              Option(rs.getInt("CTYPE")).getOrElse(0),
              Option(rs.getString("CABBREV")).getOrElse(""),
              Option(rs.getInt("CELEM_CLASS")).getOrElse(0),
              Option(rs.getDouble("CWEIGHT")).getOrElse(0.0),
              Option(rs.getInt("CSUPLID")).getOrElse(0),
              Option(rs.getInt("CAC")).getOrElse(0),
              Option(rs.getString("CMT")).getOrElse(""),
              Option(rs.getString("CSTOCK_CODE")).getOrElse(""),
              Option(rs.getDouble("XPOS")).getOrElse(0.0),
              Option(rs.getDouble("YPOS")).getOrElse(0.0),
              Option(rs.getDouble("ZPOS")).getOrElse(0.0),
              Option(rs.getDouble("XCOG")).getOrElse(0.0),
              Option(rs.getDouble("YCOG")).getOrElse(0.0),
              Option(rs.getDouble("ZCOG")).getOrElse(0.0),
              Option(rs.getString("ZONENAME")).getOrElse(""),
              Option(rs.getString("ZONEDESCR")).getOrElse(""),
              Option(rs.getString("SYSTEMNAME")).getOrElse(""),
              Option(rs.getString("SYSTEMDESCR")).getOrElse(""),
              eqStatus = status.eqStatus,
              eqStatusUser = status.eqStatusUser,
              eqStatusDate = status.eqStatusDate
            )
          }
          stmt.close()

          val sql2: String = {
            var ts: String = ""
            ret.groupBy(s => s.BSFOUNDATION).toList.foreach(t => {
              ts += "'" + t._1 + "',"
            })
            blocksSql(ts.dropRight(1))
          }
          val stmt2: Statement = connection.createStatement()
          val rs2: ResultSet = stmt2.executeQuery(sql2)
          case class BlockFoundation(block: String, foundation: String)
          val buffBlocks = ListBuffer.empty[BlockFoundation]
          while (rs2.next()) {
            buffBlocks += BlockFoundation(
              Option(rs2.getString("code")).getOrElse(""),
              Option(rs2.getString("description")).getOrElse("")
            )
          }
          stmt2.close()
          connection.close()
          val ret2 = ListBuffer.empty[EqFoundation]
          ret.foreach(item => {
            buffBlocks.find(s => s.foundation.trim.equals(item.BSFOUNDATION.trim)) match {
              case Some(v) => ret2 += item.copy(HULLBLOCK = v.block)
              case None => ret2 += item
            }
          })

          val ret3 = ListBuffer.empty[EqFoundation]

          ret2.toList.groupBy(s => s.HULLBLOCK).foreach(gr => {
            gr._2.find(s => s.eqStatus == 0) match {
              case Some(value) => {
                ret3 ++= gr._2
              }
              case None => {
                gr._2.foreach(item => {
                  ret3 += item.copy(foundationStatus = 1)
                })
              }
            }
          })

          ret3.sortBy(f => f.BSFOUNDATION).toList
        }
        catch {
          case e: Throwable =>
            connection.close()
            List.empty[EqFoundation]
        }
      }
      case None => List.empty[EqFoundation]
    }
  }

  def foranFoudationsAndEqsJson(project: String): String = foranFoudationsAndEqs(project).asJson.noSpaces


  def updateStatusSQL(project: String, bsOid: Int, user: String): Int = {

    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val data: String = s"@${user}@${(new Date()).getTime.toString}"
          val sql = updateStatus(bsOid, data)
          val result: Int = stmt.executeUpdate(sql)
          connection.commit()
          stmt.close()
          connection.close()
          result
        }
        catch {
          case e: Throwable =>
            connection.close()
            0
        }
      }
      case None => 0
    }
  }


}
