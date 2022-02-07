package local.ele.cbfill.models

import local.ele.cbfill.models.Cases.{CableBoxCable, CableBoxCables, ForanCable, ForanCableBox}
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer

trait CableCBHelper {
  private case class CableBoxCable(foranCableBox: ForanCableBox, foranCable: ForanCable)

  def allCablesSQL(): String = "select distinct    \nN.USERID,  \nN.X,   \nN.Y,  \nN.Z,\nRA.CODE,  \nRA.DESCR,\nC.CODE as CABLEINDEX,  \nCBX.DESCR as CABLEBOX,  \nCBX.SEAL_TYPE as SEAL,  \nCBX.TYPE as CBX_TYPE,\nCBX.STOCK_CODE as STOCKCODE, \nCBX.WEIGHT as WEIGHT,   \n(select O_DIAMETER from SECTION_SPEC where SPEC=C.SPEC AND NOM_SECT=C.SECT) as DIAM,\n(SELECT P.HISTORY_O as HISTO FROM NODE_PENETRATION N, PNTR_LIST P, PLS_ELEM T WHERE N.PENETRATION = T.TRAY_FITTING AND N.NODE = T.NODE1 AND T.CTYPE = 'A' AND P.SYSTEM = T.SYSTEM AND P.IDSQ = T.IDSQ  AND N.NODE=N.SEQID) as HISTO\nfrom (select SEQID,R_AREA,USERID,X,Y,Z from node where TYPE = 3 AND LENGTH(USERID)=6) N,CAB_ROUTE R,ROUT_AREA RA  , CABLE C ,NODE_PENETRATION NP, V_CABLE_PENETRATION_LIBRARY CBX\nwhere\n(N.SEQID=R.NODE1 OR N.SEQID=R.NODE2)AND  \nSUBSTR(N.USERID,1,4) = RA.CODE AND \nR.CABLE=C.SEQID AND \nN.SEQID=NP.NODE  AND \nNP.PENETRATION=CBX.OID  \norder by N.USERID"

  def allCables(project: String): List[CableBoxCables] = {
    val ret: ListBuffer[CableBoxCables] = ListBuffer.empty[CableBoxCables]
    val tmpBuf: ListBuffer[CableBoxCable] = ListBuffer.empty[CableBoxCable]
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = allCablesSQL()
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            val USERID: String = Option[String](rs.getString("USERID")).getOrElse("")
            val X: Double = Option[Double](rs.getDouble("X")).getOrElse(0.0)
            val Y: Double = Option[Double](rs.getDouble("Y")).getOrElse(0.0)
            val Z: Double = Option[Double](rs.getDouble("Z")).getOrElse(0.0)
            val CODE: String = Option[String](rs.getString("CODE")).getOrElse("")
            val DESCR: String = Option[String](rs.getString("DESCR")).getOrElse("")
            val CABLEINDEX: String = Option[String](rs.getString("CABLEINDEX")).getOrElse("")
            val CABLEBOX: String = Option[String](rs.getString("CABLEBOX")).getOrElse("")
            val SEAL: String = Option[String](rs.getString("SEAL")).getOrElse("N")
            val CBX_TYPE: Int = Option[Int](rs.getInt("CBX_TYPE")).getOrElse(1)
            val STOCKCODE: String = Option[String](rs.getString("STOCKCODE")).getOrElse("")
            val FORANWEIGHT = Option[Double](rs.getDouble("WEIGHT")).getOrElse(0.0)
            val DIAM: Double = Option[Double](rs.getDouble("DIAM")).getOrElse(0.0)
            val HISTO: String = Option[String](rs.getString("HISTO")).getOrElse("")
            val foranCableBox = ForanCableBox(USERID, X, Y, Z, CODE, DESCR, CABLEBOX, SEAL, CBX_TYPE, STOCKCODE, FORANWEIGHT, HISTO)
            tmpBuf += CableBoxCable(foranCableBox, ForanCable(CABLEINDEX, DIAM))
          }
          rs.close()
          stmt.close()
          connection.close()
          tmpBuf.toList.groupBy(p => p.foranCableBox).foreach(box => {
            ret += CableBoxCables(box._1, box._2.map(_.foranCable))
          })
          ret.toList
        }
        catch {
          case x: Throwable =>
            connection.close()
            ret.toList
        }
      }
      case None => ret.toList
    }
    ret.toList
  }

}
