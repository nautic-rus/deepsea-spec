package local.ele.eq

import breeze.linalg.{DenseMatrix, DenseVector}
import local.common.DBRequests.{MountItem, calculateH, findWorkshopMaterialContains, retrieveAllMaterialsByProject}
import local.domain.WorkShopMaterial
import local.ele.eq.EleEqManager.EleEq
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer
import scala.math.Equiv

trait EleEqHelper {

  private case class ForanEq(OID: Int, TYPE: Int, USERID: String, ZONE_SEQID: Int, ZONE_NAME: String, ZONE_DESCR: String,
                             SYSTEM_SEQID: Int, SYSTEM_NAME: String, SYSTEM_DESCR: String, ABBREV: String, WEIGHT: Double, STOCK_CODE: String, CLASS_NAME: String,
                             XCOG: Double, YCOG: Double, ZCOG: Double, EQELEC: String, A11: Double, A12: Double, A13: Double, A21: Double, A22: Double, A23: Double, A31: Double, A32: Double, A33: Double, A41: Double, A42: Double, A43: Double,
                             PX: Double, PY: Double, PZ: Double, SURFACE: String)


 // private def eqLabelSql(eqOid: String) = s"select  (select long_descr from ELEMENT_LANG where lang=-1 and elem = oid) as EQELEC from element where oid=${eqOid}"

  private def eqByOidSql(eqOid: String) = s"select \nEL.OID,\nEL.TYPE,\nEL.USERID,\nZ.SEQID as ZONE_SEQID,\nZ.NAME as ZONE_NAME,\nZL.DESCR as ZONE_DESCR,\nS.SEQID as SYSTEM_SEQID,\nS.NAME as SYSTEM_NAME,\nSL.DESCR as SYSTEM_DESCR,\nC.ABBREV,\nC.WEIGHT,\nC.STOCK_CODE,\nOC.NAME as CLASS_NAME,\nELEMPOS.XCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as XCOG,\nELEMPOS.YCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as YCOG,\nELEMPOS.ZCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as ZCOG,\n(select long_descr from ELEMENT_LANG where lang=-1 and elem = EL.OID) as EQELEC,\n  POS.A11  ,\n  POS.A12  ,\n  POS.A13  ,\n  POS.A21 ,\n  POS.A22  ,\n  POS.A23  ,\n  POS.A31  ,\n  POS.A32  ,\n  POS.A33  ,\n  POS.A41  ,\n  POS.A42  ,\n  POS.A43 ,\n  P.X * 1000 as PX, \n  P.Y * 1000 as PY,\n  P.Z * 1000 as PZ,\n    (\n         select \n         BN2.name\n         from BS_DESIGN_NODE  BDN, BS_DESIGN_ATOM BDA, BS_ATOM_FIXED_ATTRIBUTE BAF, bs_node BN, bs_node BN2\n         where \n         BDN.model_oid=EL.OID AND \n         BDA.BS_DESIGN_NODE_OID=BDN.OID AND\n         BAF.BS_DS_ATOM_OID=BDA.OID AND\n         BN.OID=BAF.BS_NODE_OID AND\n         BN2.OID=BN.parent_node\n  ) as SURFACE\nfrom element EL, COMPONENT C, OBJ_CLASS OC, MODEL M ,ELEM_POS EP, MDLCONNECTOR P,  ELEM_ABS_POS POS , ZONE Z, ZONE_LANG ZL, SYSTEMS S, SYSTEMS_LANG SL\nwhere \nEL.COMP=C.OID AND\nC.ELEM_CLASS=OC.OID AND\nC.MODEL=M.OID_F_LIB_MODEL AND\nEL.OID=EP.ELEM AND\nPOS.OID=EP.OID AND\nM.OID = P.OID_MODEL AND\nP.LABEL='MF' AND \nEL.ZONE=Z.OID AND\nEL.ZONE=ZL.ZONE AND ZL.LANG=-2 AND\nEL.SYSTEM=S.OID AND\nEL.SYSTEM=SL.SYSTEM AND SL.LANG=-2 AND\nEL.OID=${eqOid}"

  //private def testEqSQL(): String = "select \nE.OID,\nC.ABBREV ,\nC.WEIGHT,\nC.STOCK_CODE,\nE.TYPE      ,\nE.USERID    ,\nE.ZONE      ,\nE.SYSTEM    ,\n  POS.A11  ,\n  POS.A12  ,\n  POS.A13  ,\n  POS.A21 ,\n  POS.A22  ,\n  POS.A23  ,\n  POS.A31  ,\n  POS.A32  ,\n  POS.A33  ,\n  POS.A41  ,\n  POS.A42  ,\n  POS.A43 ,\n  P.X *1000 as PX, \n  P.Y * 1000 as PY,\n  P.Z * 1000 as PZ\n  from ELEMENT E,ELEM_ABS_POS POS , ELEM_POS EP, COMPONENT C, MODEL M, MDLCONNECTOR P\n  where \n  E.OID=EP.ELEM AND\n  POS.OID=EP.OID AND\n  E.COMP=C.OID AND\n  C.MODEL=M.OID_F_LIB_MODEL AND\n  M.OID = P.OID_MODEL AND\n  P.LABEL='qwer' AND\n  E.USERID ='2Я2-18'\n"


  def eqLabelsByEqOid(project: String, eqOid: String): List[String] = {
    val buff = ListBuffer.empty[String]
    val eq = eqByOID(project, eqOid)
    buff += "V=" + eq.MARGIN.toString
    buff += eq.LABEL
    eq.SUPPORTS.sortBy(s => s.label).foreach(s => {
      buff += s.label
    })
    buff.toList
  }


  def eqByOID(project: String, eqOid: String): EleEq = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(eqByOidSql(eqOid))
          if (rs.next()) {
            val eq = ForanEq(
              Option[Int](rs.getInt("OID")).getOrElse(0),
              Option[Int](rs.getInt("TYPE")).getOrElse(0),
              Option[String](rs.getString("USERID")).getOrElse(""),
              Option[Int](rs.getInt("ZONE_SEQID")).getOrElse(0),
              Option[String](rs.getString("ZONE_NAME")).getOrElse(""),
              Option[String](rs.getString("ZONE_DESCR")).getOrElse(""),
              Option[Int](rs.getInt("SYSTEM_SEQID")).getOrElse(0),
              Option[String](rs.getString("SYSTEM_NAME")).getOrElse(""),
              Option[String](rs.getString("SYSTEM_DESCR")).getOrElse(""),
              Option[String](rs.getString("ABBREV")).getOrElse(""),
              Option[Double](rs.getDouble("WEIGHT")).getOrElse(0.0),
              Option[String](rs.getString("STOCK_CODE")).getOrElse(""),
              Option[String](rs.getString("CLASS_NAME")).getOrElse(""),
              Option[Double](rs.getDouble("XCOG")).getOrElse(0.0),
              Option[Double](rs.getDouble("YCOG")).getOrElse(0.0),
              Option[Double](rs.getDouble("ZCOG")).getOrElse(0.0),
              Option[String](rs.getString("EQELEC")).getOrElse(""),
              Option[Double](rs.getDouble("A11")).getOrElse(0.0),
              Option[Double](rs.getDouble("A12")).getOrElse(0.0),
              Option[Double](rs.getDouble("A13")).getOrElse(0.0),
              Option[Double](rs.getDouble("A21")).getOrElse(0.0),
              Option[Double](rs.getDouble("A22")).getOrElse(0.0),
              Option[Double](rs.getDouble("A23")).getOrElse(0.0),
              Option[Double](rs.getDouble("A31")).getOrElse(0.0),
              Option[Double](rs.getDouble("A32")).getOrElse(0.0),
              Option[Double](rs.getDouble("A33")).getOrElse(0.0),
              Option[Double](rs.getDouble("A41")).getOrElse(0.0),
              Option[Double](rs.getDouble("A42")).getOrElse(0.0),
              Option[Double](rs.getDouble("A43")).getOrElse(0.0),
              Option[Double](rs.getDouble("PX")).getOrElse(0.0),
              Option[Double](rs.getDouble("PY")).getOrElse(0.0),
              Option[Double](rs.getDouble("PZ")).getOrElse(0.0),
              Option[String](rs.getString("SURFACE")).getOrElse("")
            )
            val wmats = retrieveAllMaterialsByProject(project)
            val label: String = calculateDrawingLabel(eq.EQELEC)
            val supports: List[MountItem] = calculateSupports(eq.EQELEC, wmats)
            val marigin: Int = calcH(eq)
            val wmat: WorkShopMaterial = findWorkshopMaterialContains(eq.STOCK_CODE, wmats)
            EleEq(eq.OID, label, eq.TYPE, eq.USERID, eq.ZONE_SEQID, eq.ZONE_NAME, eq.ZONE_DESCR, eq.SYSTEM_SEQID, eq.SYSTEM_NAME, eq.SYSTEM_DESCR, eq.ABBREV, eq.XCOG, eq.YCOG, eq.ZCOG, eq.WEIGHT, eq.STOCK_CODE, eq.CLASS_NAME, eq.SURFACE, marigin, supports, wmat)
          } else {
            EleEq()
          }
        }
        catch {
          case x: Throwable =>
            println(x.toString)
            EleEq()
        }
      }
      case None => EleEq()
    }
  }

/*
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
*/

  private def calcH(eq: ForanEq): Int = {
    val dm = DenseMatrix(
      (eq.A11, eq.A21, eq.A31, eq.A41),
      (eq.A12, eq.A22, eq.A32, eq.A42),
      (eq.A13, eq.A23, eq.A33, eq.A43),
    )
    val dv = DenseVector(eq.PX, eq.PY, eq.PZ, 1.0)
    val res = dm * dv
    val x = res.data(0)
    val y = res.data(1)
    val z = res.data(2)
    calculateH(x, y, z, eq.SURFACE)
  }

  private def calculateDrawingLabel(in: String): String = {
    if (in.nonEmpty) {
      if (in.contains('\n')) {
        val fr = in.split('\n').head
        if (fr.contains('|')) {
          fr.split('|').head
        } else {
          fr
        }
      } else {
        if (in.contains('|')) {
          in.split('|').head
        } else {
          in
        }
      }
    } else {
      "NF"
    }
  }

  private def calculateSupports(in: String, wmaterials: List[WorkShopMaterial]): List[MountItem] = {
    val buffer = ListBuffer.empty[MountItem]
    in.split('\n').foreach(row => {
      if (row.count(_ == '|') == 3) {
        val items = row.split('|')
        buffer += MountItem(findWorkshopMaterialContains(items(1), wmaterials), items(0), items(2), items(3).toDoubleOption.getOrElse(0.0), false)
      }
    })
    buffer.toList
  }
/*
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
  */
}
