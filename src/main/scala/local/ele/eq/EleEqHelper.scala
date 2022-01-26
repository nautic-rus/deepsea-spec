package local.ele.eq

import breeze.linalg.{DenseMatrix, DenseVector}
import local.common.DBRequests.{MountItem, calculateH, findWorkshopMaterialContains, listToSqlString, retrieveAllMaterialsByProject}
import local.domain.WorkShopMaterial
import local.ele.CommonEle.{EleComplect, retrieveEleComplects}
import local.ele.eq.EleEqManager.EleEq
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer


trait EleEqHelper {

  private case class ForanEq(OID: Int, TYPE: Int, USERID: String, ZONE_SEQID: Int, ZONE_NAME: String, ZONE_DESCR: String,
                             SYSTEM_SEQID: Int, SYSTEM_NAME: String, SYSTEM_DESCR: String, ABBREV: String, WEIGHT: Double, STOCK_CODE: String, CLASS_NAME: String,
                             RA_CODE: String = "", RA_DESCR: String = "", NODE_USERID: String = "", EQELEC: String,
                             XCOG: Double, YCOG: Double, ZCOG: Double, A11: Double, A12: Double, A13: Double, A21: Double, A22: Double, A23: Double, A31: Double, A32: Double, A33: Double, A41: Double, A42: Double, A43: Double,
                             PX: Double, PY: Double, PZ: Double, SURFACE: String)

  private def eqByOidSql(eqOid: String) = s"select \nEL.OID,\nEL.TYPE,\nEL.USERID,\nZ.SEQID as ZONE_SEQID,\nZ.NAME as ZONE_NAME,\nZL.DESCR as ZONE_DESCR,\nS.SEQID as SYSTEM_SEQID,\nS.NAME as SYSTEM_NAME,\nSL.DESCR as SYSTEM_DESCR,\nC.ABBREV,\nC.WEIGHT,\nC.STOCK_CODE,\nOC.NAME as CLASS_NAME,\nRR.CODE as RA_CODE,\nRR.DESCR as RA_DESCR,\nN.USERID as NODE_USERID,\n(select long_descr from ELEMENT_LANG where lang=-1 and elem = EL.OID) as EQELEC,\nELEMPOS.XCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as XCOG,\nELEMPOS.YCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as YCOG,\nELEMPOS.ZCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as ZCOG,\n  POS.A11  ,\n  POS.A12  ,\n  POS.A13  ,\n  POS.A21 ,\n  POS.A22  ,\n  POS.A23  ,\n  POS.A31  ,\n  POS.A32  ,\n  POS.A33  ,\n  POS.A41  ,\n  POS.A42  ,\n  POS.A43,\n  (select X from MDLCONNECTOR where OID_MODEL=M.OID and LABEL='MF')* 1000 as PX,\n  (select Y from MDLCONNECTOR where OID_MODEL=M.OID and LABEL='MF')* 1000 as PY,\n  (select Z from MDLCONNECTOR where OID_MODEL=M.OID and LABEL='MF')* 1000 as PZ,\n(\n         select \n         BN2.name\n         from BS_DESIGN_NODE  BDN, BS_DESIGN_ATOM BDA, BS_ATOM_FIXED_ATTRIBUTE BAF, bs_node BN, bs_node BN2\n         where \n         BDN.model_oid=EL.OID AND \n         BDA.BS_DESIGN_NODE_OID=BDN.OID AND\n         BAF.BS_DS_ATOM_OID=BDA.OID AND\n         BN.OID=BAF.BS_NODE_OID AND\n         BN2.OID=BN.parent_node\n) as SURFACE \nfrom element EL, ZONE Z, ZONE_LANG ZL, SYSTEMS S, SYSTEMS_LANG SL,ROUT_AREA RR ,NODE N,NODE_ELEM NE,\nCOMPONENT C, OBJ_CLASS OC, MODEL M ,ELEM_POS EP,  ELEM_ABS_POS POS\nwhere\nEL.OID=NE.ELEM AND\nRR.SEQID=N.R_AREA AND\nN.SEQID=NE.NODE AND\nEL.OID=EP.ELEM AND\nPOS.OID=EP.OID AND\nEL.ZONE=Z.OID AND\nEL.ZONE=ZL.ZONE AND ZL.LANG=-2 AND\nEL.SYSTEM=S.OID AND\nEL.SYSTEM=SL.SYSTEM AND SL.LANG=-2 AND\nEL.COMP=C.OID AND\nC.ELEM_CLASS=OC.OID AND\nC.MODEL=M.OID_F_LIB_MODEL AND\nEL.OID=${eqOid}"

  //private def eqByComplectsSql(compectStr: String) =s"select \nEL.OID,\nEL.TYPE,\nEL.USERID,\nZ.SEQID as ZONE_SEQID,\nZ.NAME as ZONE_NAME,\nZL.DESCR as ZONE_DESCR,\nS.SEQID as SYSTEM_SEQID,\nS.NAME as SYSTEM_NAME,\nSL.DESCR as SYSTEM_DESCR,\nC.ABBREV,\nC.WEIGHT,\nC.STOCK_CODE,\nOC.NAME as CLASS_NAME,\nRR.CODE as RA_CODE,\nRR.DESCR as RA_DESCR,\nN.USERID as NODE_USERID,\n(select long_descr from ELEMENT_LANG where lang=-1 and elem = EL.OID) as EQELEC,\nELEMPOS.XCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as XCOG,\nELEMPOS.YCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as YCOG,\nELEMPOS.ZCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as ZCOG,\n  POS.A11  ,\n  POS.A12  ,\n  POS.A13  ,\n  POS.A21 ,\n  POS.A22  ,\n  POS.A23  ,\n  POS.A31  ,\n  POS.A32  ,\n  POS.A33  ,\n  POS.A41  ,\n  POS.A42  ,\n  POS.A43,\n  (select X from MDLCONNECTOR where OID_MODEL=M.OID and LABEL='MF')* 1000 as PX,\n  (select Y from MDLCONNECTOR where OID_MODEL=M.OID and LABEL='MF')* 1000 as PY,\n  (select Z from MDLCONNECTOR where OID_MODEL=M.OID and LABEL='MF')* 1000 as PZ,\n(\n         select \n         BN2.name\n         from BS_DESIGN_NODE  BDN, BS_DESIGN_ATOM BDA, BS_ATOM_FIXED_ATTRIBUTE BAF, bs_node BN, bs_node BN2\n         where \n         BDN.model_oid=EL.OID AND \n         BDA.BS_DESIGN_NODE_OID=BDN.OID AND\n         BAF.BS_DS_ATOM_OID=BDA.OID AND\n         BN.OID=BAF.BS_NODE_OID AND\n         BN2.OID=BN.parent_node\n) as SURFACE \nfrom element EL, ZONE Z, ZONE_LANG ZL, SYSTEMS S, SYSTEMS_LANG SL,ROUT_AREA RR ,NODE N,NODE_ELEM NE,\nCOMPONENT C, OBJ_CLASS OC, MODEL M ,ELEM_POS EP,  ELEM_ABS_POS POS\nwhere\nEL.OID=NE.ELEM AND\nRR.SEQID=N.R_AREA AND\nN.SEQID=NE.NODE AND\nEL.OID=EP.ELEM AND\nPOS.OID=EP.OID AND\nEL.ZONE=Z.OID AND\nEL.ZONE=ZL.ZONE AND ZL.LANG=-2 AND\nEL.SYSTEM=S.OID AND\nEL.SYSTEM=SL.SYSTEM AND SL.LANG=-2 AND\nEL.COMP=C.OID AND\nC.ELEM_CLASS=OC.OID AND\nC.MODEL=M.OID_F_LIB_MODEL AND\nRR.code in (${compectStr}) AND EL.USERID not like '#%'  AND N.USERID not like '#%' \norder by EL.USERID"

  private def eqByComplectsSql(compectStr: String) = s"select \nEL.OID,\nEL.TYPE,\nEL.USERID,\nZ.SEQID as ZONE_SEQID,\nZ.NAME as ZONE_NAME,\nZL.DESCR as ZONE_DESCR,\nS.SEQID as SYSTEM_SEQID,\nS.NAME as SYSTEM_NAME,\nSL.DESCR as SYSTEM_DESCR,\nC.ABBREV,\nC.WEIGHT,\nC.STOCK_CODE,\nOC.NAME as CLASS_NAME,\nRR.CODE as RA_CODE,\nRR.DESCR as RA_DESCR,\nN.USERID as NODE_USERID,\n(select long_descr from ELEMENT_LANG where lang=-1 and elem = EL.OID) as EQELEC,\nELEMPOS.XCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as XCOG,\nELEMPOS.YCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as YCOG,\nELEMPOS.ZCOG (EL.OID,EL.TYPE,EL.DECK,EL.COMP) as ZCOG,\n  POS.A11  ,\n  POS.A12  ,\n  POS.A13  ,\n  POS.A21 ,\n  POS.A22  ,\n  POS.A23  ,\n  POS.A31  ,\n  POS.A32  ,\n  POS.A33  ,\n  POS.A41  ,\n  POS.A42  ,\n  POS.A43,\n  (select X from MDLCONNECTOR where OID_MODEL=M.OID and LABEL='MF')* 1000 as PX,\n  (select Y from MDLCONNECTOR where OID_MODEL=M.OID and LABEL='MF')* 1000 as PY,\n  (select Z from MDLCONNECTOR where OID_MODEL=M.OID and LABEL='MF')* 1000 as PZ,\n(\n         select \n         BN2.name\n         from BS_DESIGN_NODE  BDN, BS_DESIGN_ATOM BDA, BS_ATOM_FIXED_ATTRIBUTE BAF, bs_node BN, bs_node BN2\n         where \n         BDN.model_oid=EL.OID AND \n         BDA.BS_DESIGN_NODE_OID=BDN.OID AND\n         BAF.BS_DS_ATOM_OID=BDA.OID AND\n         BN.OID=BAF.BS_NODE_OID AND\n         BN2.OID=BN.parent_node\n) as SURFACE \nfrom element EL, ZONE Z, ZONE_LANG ZL, SYSTEMS S, SYSTEMS_LANG SL,ROUT_AREA RR ,NODE N,NODE_ELEM NE,\nCOMPONENT C, OBJ_CLASS OC, MODEL M ,ELEM_POS EP,  ELEM_ABS_POS POS , COMPONENT_ELEC CE\nwhere\nEL.OID=NE.ELEM AND\nRR.SEQID=N.R_AREA AND\nN.SEQID=NE.NODE AND\nEL.OID=EP.ELEM AND\nPOS.OID=EP.OID AND\nEL.ZONE=Z.OID AND\nEL.ZONE=ZL.ZONE AND ZL.LANG=-2 AND\nEL.SYSTEM=S.OID AND\nEL.SYSTEM=SL.SYSTEM AND SL.LANG=-2 AND\nEL.COMP=C.OID AND\nC.ELEM_CLASS=OC.OID AND\nC.MODEL=M.OID_F_LIB_MODEL AND C.OID=CE.COMP AND CE.MECHANICAL=0 AND \nRR.code in (${compectStr}) AND EL.USERID not like '#%'  AND N.USERID not like '#%' \norder by EL.USERID"


  def eqLabelsByEqOid(project: String, eqOid: String): List[String] = {
    val buff = ListBuffer.empty[String]
    val eq = eqByOID(project, eqOid)
    //buff += "V=" + eq.MARGIN.toString
    buff += eq.USERID
    buff += eq.LABEL
    /*    eq.SUPPORTS.sortBy(s => s.label).foreach(s => {
          buff += s.label
        })*/
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
              Option(rs.getInt("OID")).getOrElse(0),
              Option(rs.getInt("TYPE")).getOrElse(0),
              Option(rs.getString("USERID")).getOrElse(""),
              Option(rs.getInt("ZONE_SEQID")).getOrElse(0),
              Option(rs.getString("ZONE_NAME")).getOrElse(""),
              Option(rs.getString("ZONE_DESCR")).getOrElse(""),
              Option(rs.getInt("SYSTEM_SEQID")).getOrElse(0),
              Option(rs.getString("SYSTEM_NAME")).getOrElse(""),
              Option(rs.getString("SYSTEM_DESCR")).getOrElse(""),
              Option(rs.getString("ABBREV")).getOrElse(""),
              Option(rs.getDouble("WEIGHT")).getOrElse(0.0),
              Option(rs.getString("STOCK_CODE")).getOrElse(""),
              Option(rs.getString("CLASS_NAME")).getOrElse(""),
              Option(rs.getString("RA_CODE")).getOrElse(""),
              Option(rs.getString("RA_DESCR")).getOrElse(""),
              Option(rs.getString("NODE_USERID")).getOrElse(""),
              Option(rs.getString("EQELEC")).getOrElse(""),
              Option(rs.getDouble("XCOG")).getOrElse(0.0),
              Option(rs.getDouble("YCOG")).getOrElse(0.0),
              Option(rs.getDouble("ZCOG")).getOrElse(0.0),
              Option(rs.getDouble("A11")).getOrElse(0.0),
              Option(rs.getDouble("A12")).getOrElse(0.0),
              Option(rs.getDouble("A13")).getOrElse(0.0),
              Option(rs.getDouble("A21")).getOrElse(0.0),
              Option(rs.getDouble("A22")).getOrElse(0.0),
              Option(rs.getDouble("A23")).getOrElse(0.0),
              Option(rs.getDouble("A31")).getOrElse(0.0),
              Option(rs.getDouble("A32")).getOrElse(0.0),
              Option(rs.getDouble("A33")).getOrElse(0.0),
              Option(rs.getDouble("A41")).getOrElse(0.0),
              Option(rs.getDouble("A42")).getOrElse(0.0),
              Option(rs.getDouble("A43")).getOrElse(0.0),
              Option(rs.getDouble("PX")).getOrElse(0.0),
              Option(rs.getDouble("PY")).getOrElse(0.0),
              Option(rs.getDouble("PZ")).getOrElse(0.0),
              Option(rs.getString("SURFACE")).getOrElse("")
            )
            val wmats = retrieveAllMaterialsByProject(project)
            val label: String = calculateDrawingLabel(eq.EQELEC)
            val supports: List[MountItem] = calculateSupports(eq.EQELEC, wmats)
            val userId: String = if (eq.NODE_USERID.equals(eq.USERID)) eq.USERID else eq.NODE_USERID
            val marigin: Int = calcH(eq)
            val wmat: WorkShopMaterial = findWorkshopMaterialContains(eq.STOCK_CODE, wmats)
            rs.close()
            stmt.close()
            connection.close()
            EleEq(eq.OID, label, eq.TYPE, userId, eq.ZONE_SEQID, eq.ZONE_NAME, eq.ZONE_DESCR, eq.SYSTEM_SEQID, eq.SYSTEM_NAME, eq.SYSTEM_DESCR, eq.ABBREV, eq.XCOG, eq.YCOG, eq.ZCOG, eq.WEIGHT, eq.STOCK_CODE, eq.CLASS_NAME, eq.SURFACE, marigin, supports, wmat)
          } else {
            rs.close()
            stmt.close()
            connection.close()
            EleEq()
          }
        }
        catch {
          case x: Throwable =>
            println(x.toString)
            connection.close()
            EleEq()
        }
      }
      case None => EleEq()
    }
  }

  def eqsByComplect(project: String, complect: EleComplect): List[EleEq] = {
    val buff = ListBuffer.empty[EleEq]
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql=eqByComplectsSql(listToSqlString(complect.zoneNames))
          val rs: ResultSet = stmt.executeQuery(eqByComplectsSql(listToSqlString(complect.zoneNames)))
          val wmats = retrieveAllMaterialsByProject(project)
          while (rs.next()) {
            val eq = ForanEq(
              Option(rs.getInt("OID")).getOrElse(0),
              Option(rs.getInt("TYPE")).getOrElse(0),
              Option(rs.getString("USERID")).getOrElse(""),
              Option(rs.getInt("ZONE_SEQID")).getOrElse(0),
              Option(rs.getString("ZONE_NAME")).getOrElse(""),
              Option(rs.getString("ZONE_DESCR")).getOrElse(""),
              Option(rs.getInt("SYSTEM_SEQID")).getOrElse(0),
              Option(rs.getString("SYSTEM_NAME")).getOrElse(""),
              Option(rs.getString("SYSTEM_DESCR")).getOrElse(""),
              Option(rs.getString("ABBREV")).getOrElse(""),
              Option(rs.getDouble("WEIGHT")).getOrElse(0.0),
              Option(rs.getString("STOCK_CODE")).getOrElse(""),
              Option(rs.getString("CLASS_NAME")).getOrElse(""),
              Option(rs.getString("RA_CODE")).getOrElse(""),
              Option(rs.getString("RA_DESCR")).getOrElse(""),
              Option(rs.getString("USERID")).getOrElse(""),
              Option(rs.getString("EQELEC")).getOrElse(""),
              Option(rs.getDouble("XCOG")).getOrElse(0.0),
              Option(rs.getDouble("YCOG")).getOrElse(0.0),
              Option(rs.getDouble("ZCOG")).getOrElse(0.0),
              Option(rs.getDouble("A11")).getOrElse(0.0),
              Option(rs.getDouble("A12")).getOrElse(0.0),
              Option(rs.getDouble("A13")).getOrElse(0.0),
              Option(rs.getDouble("A21")).getOrElse(0.0),
              Option(rs.getDouble("A22")).getOrElse(0.0),
              Option(rs.getDouble("A23")).getOrElse(0.0),
              Option(rs.getDouble("A31")).getOrElse(0.0),
              Option(rs.getDouble("A32")).getOrElse(0.0),
              Option(rs.getDouble("A33")).getOrElse(0.0),
              Option(rs.getDouble("A41")).getOrElse(0.0),
              Option(rs.getDouble("A42")).getOrElse(0.0),
              Option(rs.getDouble("A43")).getOrElse(0.0),
              Option(rs.getDouble("PX")).getOrElse(0.0),
              Option(rs.getDouble("PY")).getOrElse(0.0),
              Option(rs.getDouble("PZ")).getOrElse(0.0),
              Option(rs.getString("SURFACE")).getOrElse("")
            )
            val label: String = calculateDrawingLabel(eq.EQELEC)
            val supports: List[MountItem] = calculateSupports(eq.EQELEC, wmats)
            val marigin: Int = calcH(eq)
            val userId: String = if (eq.NODE_USERID.equals(eq.USERID)) eq.USERID else eq.NODE_USERID
            val wmat: WorkShopMaterial = findWorkshopMaterialContains(eq.STOCK_CODE, wmats)
            buff += EleEq(eq.OID, label, eq.TYPE, userId, eq.ZONE_SEQID, eq.ZONE_NAME, eq.ZONE_DESCR, eq.SYSTEM_SEQID, eq.SYSTEM_NAME, eq.SYSTEM_DESCR, eq.ABBREV, eq.XCOG, eq.YCOG, eq.ZCOG, eq.WEIGHT, eq.STOCK_CODE, eq.CLASS_NAME, eq.SURFACE, marigin, supports, wmat)
          }
          rs.close()
          stmt.close()
          connection.close()
          buff.toList
        }
        catch {
          case x: Throwable =>
            connection.close()
            buff.toList
        }
      }
      case None => buff.toList
    }
  }

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

}
