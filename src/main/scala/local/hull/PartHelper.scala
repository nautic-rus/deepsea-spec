package local.hull

import local.hull.PartManager.{PartLabel, PrdPart}
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer

trait PartHelper {

  private def partsByDrawingNum(drNum: String): String = s"select\n(select OID from V_PRD_PART where BLOCK_NAME=PNL.BLOCK_CODE AND PART_NAME=PNL.PART_CODE) as OID,\nPNL.PART_CODE,\nPNL.PART_TYPE,\nPNL.BLOCK_CODE,\nB.DESCRIPTION,\nPNL.NUM_EQ_PART,\nPNL.PART_DESC,\nPNL.ELEM_TYPE,\nPNL.MATERIAL,\nPNL.LENGTH,\nPNL.WIDTH,\nPNL.THICKNESS,\nPNL.WEIGHT_UNIT,\nPNL.TOTAL_WEIGHT,\nPNL.NEST_ID,\nPNL.NUM_PART_NEST,\nPNL.NEST_LENGTH,\nPNL.NEST_WIDTH,\nPNL.NUM_EQ_NEST,\n(select KSE_KPL from V_PRD_PLPRF_MAT where NEST_ID=PNL.NEST_ID) as KSE_KPL,\n(select STOCK_CODE0 from STD_PROFILE where kse=(select KSE_KPL from V_PRD_PLPRF_MAT where NEST_ID=PNL.NEST_ID)) as STOCK1,\n(select STORAGE_CODE from STD_PLATE where kpl=(select KSE_KPL from V_PRD_PLPRF_MAT where NEST_ID=PNL.NEST_ID)) as STOCK2\nfrom V_PRD_PART_NEST_LIST PNL, BLOCK B\nwhere \nPNL.BLOCK_CODE=B.CODE AND\nB.DESCRIPTION like '%${drNum}%'"

  private def partsByDrawingNumAndPartName(drNum: String, partName: String) = s"select distinct\nPNL.PART_CODE,\nPNL.PART_TYPE,\nPNL.BLOCK_CODE,\nB.DESCRIPTION,\nPNL.NUM_EQ_PART,\nPNL.PART_DESC,\nPNL.ELEM_TYPE,\nPNL.MATERIAL,\nPNL.LENGTH,\nPNL.WIDTH,\nPNL.THICKNESS,\nPNL.WEIGHT_UNIT,\nPNL.TOTAL_WEIGHT,\nPNL.NUM_PART_NEST\nfrom V_PRD_PART_NEST_LIST PNL, BLOCK B\nwhere \nPNL.BLOCK_CODE=B.CODE AND\nB.DESCRIPTION like '%${drNum}%' \nAND PNL.PART_CODE='${partName}'"


  def ForanPartLabelByDrawingNumAndPartName(project: String, drNum: String, partName: String): PartLabel = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = partsByDrawingNumAndPartName(drNum, partName)
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = {
            if (rs.next()) {
              PartLabel(
                Option[String](rs.getString("PART_CODE")).getOrElse(""),
                Option[Int](rs.getInt("PART_TYPE")).getOrElse(0),
                Option[String](rs.getString("BLOCK_CODE")).getOrElse(""),
                Option[String](rs.getString("DESCRIPTION")).getOrElse(""),
                Option[Int](rs.getInt("NUM_EQ_PART")).getOrElse(0),
                Option[String](rs.getString("PART_DESC")).getOrElse(""),
                Option[String](rs.getString("ELEM_TYPE")).getOrElse(""),
                Option[String](rs.getString("MATERIAL")).getOrElse(""),
                Option[Double](rs.getDouble("LENGTH")).getOrElse(0.0),
                Option[Double](rs.getDouble("WIDTH")).getOrElse(0.0),
                Option[Double](rs.getDouble("THICKNESS")).getOrElse(0.0),
                Option[Double](rs.getDouble("WEIGHT_UNIT")).getOrElse(0.0),
                Option[Double](rs.getDouble("TOTAL_WEIGHT")).getOrElse(0.0),
                Option[Int](rs.getInt("NUM_PART_NEST")).getOrElse(0),
              )
            } else {
              PartLabel()
            }
          }
          stmt.close()
          connection.close()
          ret
        }
        catch {
          case _: Throwable => PartLabel()
        }
      }
      case None => PartLabel()
    }
  }

  def ForanPartsByDrawingNum(project: String, drNum: String): List[PrdPart] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = partsByDrawingNum(drNum)
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[PrdPart]
          while (rs.next()) {
            ret += PrdPart(
              Option[Int](rs.getInt("OID")).getOrElse(0),
              Option[String](rs.getString("PART_CODE")).getOrElse(""),
              Option[Int](rs.getInt("PART_TYPE")).getOrElse(0),
              Option[String](rs.getString("BLOCK_CODE")).getOrElse(""),
              Option[String](rs.getString("DESCRIPTION")).getOrElse(""),
              Option[Int](rs.getInt("NUM_EQ_PART")).getOrElse(0),
              Option[String](rs.getString("PART_DESC")).getOrElse(""),
              Option[String](rs.getString("ELEM_TYPE")).getOrElse(""),
              Option[String](rs.getString("MATERIAL")).getOrElse(""),
              Option[Double](rs.getDouble("LENGTH")).getOrElse(0.0),
              Option[Double](rs.getDouble("WIDTH")).getOrElse(0.0),
              Option[Double](rs.getDouble("THICKNESS")).getOrElse(0.0),
              Option[Double](rs.getDouble("WEIGHT_UNIT")).getOrElse(0.0),
              Option[Double](rs.getDouble("TOTAL_WEIGHT")).getOrElse(0.0),
              Option[String](rs.getString("NEST_ID")).getOrElse(""),
              Option[Int](rs.getInt("NUM_PART_NEST")).getOrElse(0),

              Option[Double](rs.getDouble("NEST_LENGTH")).getOrElse(0.0),
              Option[Double](rs.getDouble("NEST_WIDTH")).getOrElse(0.0),
              Option[Int](rs.getInt("NUM_EQ_NEST")).getOrElse(0),
              Option[Int](rs.getInt("KSE_KPL")).getOrElse(0),
              {
                Option[String](rs.getString("STOCK1")) match {
                  case Some(value) =>
                    if (value.nonEmpty) {
                      value
                    }
                    else {
                      Option[String](rs.getString("STOCK2")) match {
                        case Some(value) => if (value.nonEmpty) value else ""
                        case None => ""
                      }
                    }
                  case None => Option[String](rs.getString("STOCK2")) match {
                    case Some(value) => if (value.nonEmpty) value else ""
                    case None => ""
                  }
                }
              }
            )
          }

          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable => List.empty[PrdPart]
        }
      }
      case None => List.empty[PrdPart]
    }
  }


}
