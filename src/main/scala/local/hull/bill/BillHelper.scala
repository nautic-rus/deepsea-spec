package local.hull.bill

import local.hull.bill.BillManager.{PlateNestBill, ProfileNestBill}
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer

trait BillHelper {

  def genProfileNestBillSql(): String = "select \n    KSE,\n    KQ,\n    TP,\n    WH,\n    WT,\n    FH,\n    FT,\n    NESTID,\n    NP,\n    NGB,\n    NETLEN,\n    GROLEN,\n    TONETLEN,\n    TOGROLEN,\n    TONETWGT,\n    TOGROWGT,\n    SCRAP,\n    BLOCK,\n    BLOCK_OID,\n    TOTAL_BL_SCRAP,\n    TOTAL_KSE_SCRAP\n    from V_PRD_PRF_MAT"

  def genPlateNestBillSql(): String = "select\n    KPL,\n    KQ,\n    L,\n    W,\n    T,\n    NESTID,\n    NP,\n    NGP,\n    TONETWGT,\n    TOGROWGT,\n    SCRAP,\n    BLOCK,\n    BLOCK_OID,\n    TOTAL_BL_SCRAP,\n    TOTAL_KPL_SCRAP,\n    ENCLOS_TYPE,\n    ASPECT_RATIO\n    from\n    V_PRD_PLATE_MAT"

  def genProfileNestBill(project: String): List[ProfileNestBill] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = genProfileNestBillSql()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[ProfileNestBill]
          while (rs.next()) {
            ret += ProfileNestBill(
              Option(rs.getInt("KSE")).getOrElse(0),
              Option(rs.getString("KQ")).getOrElse(""),
              Option(rs.getString("TP")).getOrElse(""),
              Option(rs.getDouble("WH")).getOrElse(0.0),
              Option(rs.getDouble("WT")).getOrElse(0.0),
              Option(rs.getDouble("FH")).getOrElse(0.0),
              Option(rs.getDouble("FT")).getOrElse(0.0),
              Option(rs.getString("NESTID")).getOrElse(""),
              Option(rs.getInt("NP")).getOrElse(0),
              Option(rs.getInt("NGB")).getOrElse(0),
              Option(rs.getDouble("NETLEN")).getOrElse(0.0),
              Option(rs.getDouble("GROLEN")).getOrElse(0.0),
              Option(rs.getDouble("TONETLEN")).getOrElse(0.0),
              Option(rs.getDouble("TOGROLEN")).getOrElse(0.0),
              Option(rs.getDouble("TONETWGT")).getOrElse(0.0),
              Option(rs.getDouble("TOGROWGT")).getOrElse(0.0),
              Option(rs.getDouble("SCRAP")).getOrElse(0.0),
              Option(rs.getString("BLOCK")).getOrElse(""),
              Option(rs.getInt("BLOCK_OID")).getOrElse(0),
              Option(rs.getDouble("TOTAL_BL_SCRAP")).getOrElse(0.0),
              Option(rs.getDouble("TOTAL_KSE_SCRAP")).getOrElse(0.0)
            )
          }

          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[ProfileNestBill]
        }
      }
      case None => List.empty[ProfileNestBill]
    }
  }

  def genPlateNestBill(project: String): List[PlateNestBill] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = genPlateNestBillSql()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[PlateNestBill]
          while (rs.next()) {
            ret += PlateNestBill(
              Option(rs.getInt("KPL")).getOrElse(0),
              Option(rs.getString("KQ")).getOrElse(""),
              Option(rs.getDouble("L")).getOrElse(0.0),
              Option(rs.getDouble("W")).getOrElse(0.0),
              Option(rs.getDouble("T")).getOrElse(0.0),
              Option(rs.getString("NESTID")).getOrElse(""),
              Option(rs.getInt("NP")).getOrElse(0),
              Option(rs.getInt("NGP")).getOrElse(0),
              Option(rs.getDouble("TONETWGT")).getOrElse(0.0),
              Option(rs.getDouble("TOGROWGT")).getOrElse(0.0),
              Option(rs.getDouble("SCRAP")).getOrElse(0.0),
              Option(rs.getString("BLOCK")).getOrElse(""),
              Option(rs.getInt("BLOCK_OID")).getOrElse(0),
              Option(rs.getDouble("TOTAL_BL_SCRAP")).getOrElse(0.0),
              Option(rs.getDouble("TOTAL_KPL_SCRAP")).getOrElse(0.0),
              Option(rs.getInt("ENCLOS_TYPE")).getOrElse(0),
              Option(rs.getInt("ASPECT_RATIO")).getOrElse(0)
            )
          }

          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[PlateNestBill]
        }
      }
      case None => List.empty[PlateNestBill]
    }
  }


  //def totalNestPlates(project: String)



}
