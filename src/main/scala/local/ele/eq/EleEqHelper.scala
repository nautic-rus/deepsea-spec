package local.ele.eq

import breeze.linalg.{DenseMatrix, DenseVector}
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer
import scala.math.Equiv

trait EleEqHelper {

  private def eqLabelSql(eqOid: String) = s"select  (select long_descr from ELEMENT_LANG where lang=-1 and elem = oid) as EQELEC from element where oid=${eqOid}"

  private def testEqSQL(): String = {
    "select \nE.OID,\nC.ABBREV ,\nC.WEIGHT,\nC.STOCK_CODE,\nE.TYPE      ,\nE.USERID    ,\nE.ZONE      ,\nE.SYSTEM    ,\n  POS.A11  ,\n  POS.A12  ,\n  POS.A13  ,\n  POS.A21 ,\n  POS.A22  ,\n  POS.A23  ,\n  POS.A31  ,\n  POS.A32  ,\n  POS.A33  ,\n  POS.A41  ,\n  POS.A42  ,\n  POS.A43 ,\n  P.X *1000 as PX, \n  P.Y * 1000 as PY,\n  P.Z * 1000 as PZ\n  from ELEMENT E,ELEM_ABS_POS POS , ELEM_POS EP, COMPONENT C, MODEL M, MDLCONNECTOR P\n  where \n  E.OID=EP.ELEM AND\n  POS.OID=EP.OID AND\n  E.COMP=C.OID AND\n  C.MODEL=M.OID_F_LIB_MODEL AND\n  M.OID = P.OID_MODEL AND\n  P.LABEL='qwer' AND\n  E.USERID ='2Я2-18'\n"
  }

  def eqLabelsByEqOid(project: String, eqOid: String): List[String] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(eqLabelSql(eqOid))
          if (rs.next()) {
            extractEQLabels(Option[String](rs.getString("EQELEC")).getOrElse(""))
          } else {
            List("NF")
          }
        }
        catch {
          case _: Throwable => List("NF")
        }
      }
      case None => List("NF")
    }
  }

  private def extractEQLabels(in: String): List[String] = {
    if (in.nonEmpty) {
      if (in.contains('\n')) {
        val buff = ListBuffer.empty[String]
        in.split('\n').foreach(line => {
          if (line.contains('|')) {
            buff += line.split('|').headOption.getOrElse("NF")
          } else {
            buff += line
          }
        })
        buff.toList
      } else {
        List(in)
      }
    } else {
      List("NF")
    }
  }

  def testCoord(): (DenseMatrix[Double], DenseVector[Double]) = {
    val retM: DenseMatrix[Double] = DenseMatrix.zeros(4, 4)
    val retV: DenseVector[Double] = DenseVector.zeros(3)
    ConnectionManager.connectionByProject("P701") match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(testEqSQL())
          if (rs.next()) {

            val A11: Double = Option[Double](rs.getDouble("A11")).getOrElse(0)
            val A12: Double = Option[Double](rs.getDouble("A12")).getOrElse(0)
            val A13: Double = Option[Double](rs.getDouble("A13")).getOrElse(0)
            val A21: Double = Option[Double](rs.getDouble("A21")).getOrElse(0)
            val A22: Double = Option[Double](rs.getDouble("A22")).getOrElse(0)
            val A23: Double = Option[Double](rs.getDouble("A23")).getOrElse(0)
            val A31: Double = Option[Double](rs.getDouble("A31")).getOrElse(0)
            val A32: Double = Option[Double](rs.getDouble("A32")).getOrElse(0)
            val A33: Double = Option[Double](rs.getDouble("A33")).getOrElse(0)
            val A41: Double = Option[Double](rs.getDouble("A41")).getOrElse(0)
            val A42: Double = Option[Double](rs.getDouble("A42")).getOrElse(0)
            val A43: Double = Option[Double](rs.getDouble("A43")).getOrElse(0)

            val dm = DenseMatrix(
              (A11, A21, A31, A41),
              (A12, A22, A32, A42),
              (A13, A23, A33, A43),
            )

            val PX: Double = Option[Double](rs.getDouble("PX")).getOrElse(0)
            val PY: Double = Option[Double](rs.getDouble("PY")).getOrElse(0)
            val PZ: Double = Option[Double](rs.getDouble("PZ")).getOrElse(0)

            val dv = DenseVector(PX, PY, PZ, 1.0)

            (dm, dv)


          } else {
            (retM, retV)
          }
        }
        catch {
          case _: Throwable => (retM, retV)
        }
      }
      case None => (retM, retV)
    }
  }
}
