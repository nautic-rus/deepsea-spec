package local.ele.cb

import local.ele.cb.CableBoxManager.ForanCableBox
import local.sql.ConnectionManager
import java.sql.{ResultSet, Statement}

trait CableBoxHelper {

  //def sqlCBBySQID(CBIdsq: String) = s"select  PE.IDSQ, PE.X_COG, PE.Y_COG, PE.Z_COG,PE.WEIGHT, PE.NODE1, PE.NODE2, PL.TYPE, PL.SEAL_TYPE, PL.CODE, PL.DESCR, PL.STOCK_CODE, (select userid from pntr_list where FITT_OID=PE.IDSQ) as PENRTRATION from PLS_ELEM PE, V_CABLE_PENETRATION_LIBRARY PL where  PE.TRAY_FITTING=PL.OID AND  PE.IDSQ=${CBIdsq}"
  //def sqlCBBySQID(CBIdsq: String) = s"select  PE.IDSQ, (select USERID from ELEMENT where UUID=PE.UUID) as USERID, PE.X_COG, PE.Y_COG, PE.Z_COG, PE.NODE1, PE.NODE2, PL.TYPE, PL.SEAL_TYPE, PL.CODE, PL.DESCR, PL.STOCK_CODE from PLS_ELEM PE, V_CABLE_PENETRATION_LIBRARY PL where  PE.TRAY_FITTING=PL.OID AND PE.IDSQ=${CBIdsq}"
  def sqlCBBySQID(CBIdsq: String) = s"select   PE.IDSQ,  (select USERID from ELEMENT where UUID=PE.UUID) as USERID, PE.X_COG,  PE.Y_COG,  PE.Z_COG, PE.WEIGHT,  PE.NODE1,  PE.NODE2,  PL.TYPE,  PL.SEAL_TYPE,  PL.CODE,  PL.DESCR,  PL.STOCK_CODE,  (select userid from pntr_list where FITT_OID=PE.IDSQ) as PENRTRATION  from PLS_ELEM PE, V_CABLE_PENETRATION_LIBRARY PL where   PE.TRAY_FITTING=PL.OID AND   PE.IDSQ=${CBIdsq}"

  def ForanCableBoxBySeqId(project: String, cbIdSeq: String): ForanCableBox = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = sqlCBBySQID(cbIdSeq)
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret={
            if (rs.next()) {
              ForanCableBox(
                Option(rs.getInt("IDSQ")).getOrElse(0),
                Option(rs.getString("USERID")).getOrElse(""),
                Option(rs.getDouble("X_COG")).getOrElse(0),
                Option(rs.getDouble("Y_COG")).getOrElse(0),
                Option(rs.getDouble("Z_COG")).getOrElse(0),
                Option(rs.getDouble("WEIGHT")).getOrElse(0),
                Option(rs.getInt("NODE1")).getOrElse(0),
                Option(rs.getInt("NODE2")).getOrElse(0),
                Option(rs.getInt("TYPE")).getOrElse(0),
                Option(rs.getString("SEAL_TYPE")).getOrElse(""),
                Option(rs.getString("CODE")).getOrElse(""),
                Option(rs.getString("DESCR")).getOrElse(""),
                Option(rs.getString("STOCK_CODE")).getOrElse(""),
                Option(rs.getString("PENRTRATION")).getOrElse("")
              )
            }else{
              ForanCableBox()
            }
          }
          stmt.close()
          connection.close()
          ret
        }
        catch {
          case _: Throwable =>
            connection.close()
            ForanCableBox()
        }
      }
      case None =>  ForanCableBox()
    }
  }


}
