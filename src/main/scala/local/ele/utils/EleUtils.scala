package local.ele.utils

import local.ele.CommonEle.retrieveEleComplects
import local.sql.ConnectionManager

import java.sql.Statement

object EleUtils {

  private def sqlFixFbsOld(zoneName: String, systemName: String): String = s"update BS_DESIGN_NODE set \nMODEL_OID=\n(select M2 from \n        (select \n          DN.OID as DNOID,\n          DN.TYPE,\n          DN.MODEL_OID as M1,\n          PS.OID as M2,\n          " +
    s"DN.NAME,\n          DN.DESCRIPTION,\n          PS.TYPE,\n          PS.ZONE,\n          PS.SYSTEM,\n          PS.LINE ,\n          PS.SQID,\n          PS.BDAPFIT   ,\n          PS.BDATRI,\n          PS.UUID,\n          PS.LINK_OID\n          " +
    s"from  BS_DESIGN_NODE DN ,PIPELINE_SEGMENT PS\n          where \n            PS.zone =(select seqid from zone where userid ='${zoneName}')  and \n            PS.system=(select seqid from SYSTEMS where name ='${systemName}') AND\n          " +
    s" (((PS.line||'-'||PS.line||'-'||PS.SQID) = DN.name) OR ((PS.line||'-'||PS.line||'-0'||PS.SQID) = DN.name))" +
    s"AND\n          DN.MODEL_OID <> PS.OID\n        ) " +
    s"where DNOID = oid\n)\n\nwhere oid in (\nselect \n  DN.OID\n  from  BS_DESIGN_NODE DN ,PIPELINE_SEGMENT PS\n  where\n  " +
    s"PS.zone =(select seqid from zone where userid ='${zoneName}')  and \n  PS.system=(select seqid from SYSTEMS where name ='${systemName}') AND\n  " +
    s"(((PS.line||'-'||PS.line||'-'||PS.SQID) = DN.name) OR ((PS.line||'-'||PS.line||'-0'||PS.SQID) = DN.name)) " +
    s"AND\n  DN.MODEL_OID <> PS.OID\n)"

  private def sqlFixFbs(zoneName: String, systemName: String): String = s"update BS_DESIGN_NODE set \nMODEL_OID=\n(select M2 from \n        (select \n          DN.OID as DNOID,\n          " +
    s"DN.TYPE,\n          DN.MODEL_OID as M1,\n          PS.OID as M2,\n          DN.NAME,\n          DN.DESCRIPTION,\n          PS.TYPE,\n          PS.ZONE,\n          PS.SYSTEM,\n          " +
    s"PS.LINE ,\n          PS.SQID,\n          PS.BDAPFIT   ,\n          PS.BDATRI,\n          PS.UUID,\n          PS.LINK_OID\n          from  BS_DESIGN_NODE DN ,PIPELINE_SEGMENT PS\n          " +
    s"where \n            PS.zone =(select seqid from zone where userid in('${zoneName}'))  and \n            PS.system=(select seqid from SYSTEMS where name in('${systemName}') ) AND\n              " +
    s"(\n  (PS.line||'-'||PS.line||'-'||PS.SQID) = DN.name OR \n  ((select name from zone where seqid=PS.zone)||'-'||PS.line||'-'||PS.SQID )= DN.name OR\n   (PS.line||'-'||PS.line||'-0'||PS.SQID) = DN.name OR \n  " +
    s"((select name from zone where seqid=PS.zone)||'-'||PS.line||'-0'||PS.SQID )= DN.name\n  ) \n          AND\n          DN.MODEL_OID <> PS.OID\n        ) where DNOID = oid\n)\nwhere oid in (\nselect \n  DN.OID\n  " +
    s"from  BS_DESIGN_NODE DN ,PIPELINE_SEGMENT PS\n  where\n  PS.zone =(select seqid from zone where userid in('${zoneName}'))  and \n  PS.system=(select seqid from SYSTEMS where name in('${systemName}') ) AND\n    " +
    s"(\n  (PS.line||'-'||PS.line||'-'||PS.SQID) = DN.name OR \n  ((select name from zone where seqid=PS.zone)||'-'||PS.line||'-'||PS.SQID )= DN.name OR\n   (PS.line||'-'||PS.line||'-0'||PS.SQID) = DN.name OR \n  " +
    s"((select name from zone where seqid=PS.zone)||'-'||PS.line||'-0'||PS.SQID )= DN.name\n  ) \n  AND\n  DN.MODEL_OID <> PS.OID AND bs_design_node_adn_from_oid(DN.OID) LIKE '%${systemName}%')"


  def fixFBS(project: String, complectName: String): Unit = {

    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          retrieveEleComplects(project).find(s => s.drawingId.equals(complectName)) match {
            case Some(complect) => {
              connection.setAutoCommit(false)
              val stmt: Statement = connection.createStatement()
              complect.systemNames.foreach(system => {
                complect.zoneNames.foreach(zone => {
                  val sql = sqlFixFbs(zone, system)
                  val f: Int =stmt.executeUpdate(sql)
                  //println(zone+" "+system+" "+f.toString)

                  connection.commit()
                  stmt.close()
                  connection.close()
                })
              })
            }
            case None => connection.close()
          }
          connection.close()
        }
        catch {
          case x: Throwable => connection.close()
        }

      }
      case None => None
    }



  }

}
