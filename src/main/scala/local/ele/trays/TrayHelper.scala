package local.ele.trays

import deepsea.App
import local.common.Codecs
import local.common.DBRequests.{calculateH, listToSqlString}
import local.domain.WorkShopMaterial
import local.ele.CommonEle.EleComplect
import local.ele.trays.TrayManager.{ForanCBX, ForanTray, TrayMountData, TrayMountRules}
import local.sql.{ConnectionManager, MongoDB}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}
import org.mongodb.scala.model.Filters.{and, equal}

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}
import org.mongodb.scala.bson.codecs.Macros._

trait TrayHelper extends Codecs {

  /*  private def traySQL(trayIdsq: String): String = {
      s"select   \n  PE.IDSQ,\n  PS.OID as FDS_MODEL,\n  Z.NAME  as ZONE,\n  SYS.NAME  as SYSTEM,\n  PE.LINE,\n  PE.PLS,\n  PE.ELEM,\n  PE.WEIGHT,\n  PE.X_COG,\n  PE.Y_COG,\n  PE.Z_COG,\n  PE.CTYPE,\n  PE.TYPE,\n  N1.USERID  as NODE1,\n   N2.USERID  as NODE2,\n  PE.TRAY_LEVEL,\n  TR.STOCK_CODE,\n  N1.X *1000 as N1X,\n  N1.Y *1000 as N1Y,\n  N1.Z *1000 as N1Z,\n  N2.X *1000 as N2X,\n  N2.Y *1000 as N2Y,\n  N2.Z *1000 as N2Z,\n  SQRT( (N2.X-N1.X)*(N2.X-N1.X) + (N2.Y-N1.Y)*(N2.Y-N1.Y) + (N2.Z-N1.Z)*(N2.Z-N1.Z) )*1000 as LEN,\n  --PE.UUID,\n  (select name from bs_node where oid =(\n       select parent_node from bs_node where OID =(\n            select BS_NODE_OID  from BS_ATOM_FIXED_ATTRIBUTE where BS_DS_ATOM_OID=(\n            select oid from BS_DESIGN_ATOM where BS_DESIGN_NODE_OID=(\n                select oid from BS_DESIGN_NODE where model_oid=PS.OID)\n        )\n   )\n)) as surface   \n  from PLS_ELEM PE ,PIPELINE_SEGMENT PS, SEGMENT S, V_CTRAY_PATTERN_LEVEL TR, NODE N1, NODE N2, ZONE Z, SYSTEMS SYS\n  where \n  IDSQ = ${trayIdsq} AND\n  PE.TYPE=PS.TYPE AND PE.ZONE=PS.ZONE AND PE.SYSTEM=PS.SYSTEM AND PE.LINE=PS.LINE AND PE.PLS=PS.SQID AND\n  ((S.NODE1=PE.NODE1 AND S.NODE2=PE.NODE2) OR (S.NODE1=PE.NODE2 AND S.NODE2=PE.NODE1)) AND\n  S.PATTERN=TR.SEQID AND\n  PE.NODE1=N1.SEQID AND PE.NODE2=N2.SEQID AND\n  Z.SEQID=PE.ZONE AND\n  SYS.SEQID=PE.SYSTEM \n  "
    }*/

  private def traySQL(trayIdsq: String): String = {
    s"select   \n  PE.IDSQ,\n  PS.OID as FDS_MODEL,\n  Z.NAME  as ZONE,\n  SYS.NAME  as SYSTEM,\n  PE.LINE,\n  PE.PLS,\n  PE.ELEM,\n  PE.WEIGHT,\n  PE.X_COG,\n  PE.Y_COG,\n  PE.Z_COG,\n  PE.CTYPE,\n  PE.TYPE,\n  N1.USERID  as NODE1,\n   N2.USERID  as NODE2,\n  PE.TRAY_LEVEL,\n  TR.STOCK_CODE,\n  N1.X *1000 as N1X,\n  N1.Y *1000 as N1Y,\n  N1.Z *1000 as N1Z,\n  N2.X *1000 as N2X,\n  N2.Y *1000 as N2Y,\n  N2.Z *1000 as N2Z,\n  SQRT( (N2.X-N1.X)*(N2.X-N1.X) + (N2.Y-N1.Y)*(N2.Y-N1.Y) + (N2.Z-N1.Z)*(N2.Z-N1.Z) )*1000 as LEN,\n  (\n         select \n         BN2.name\n         from BS_DESIGN_NODE  BDN, BS_DESIGN_ATOM BDA, BS_ATOM_FIXED_ATTRIBUTE BAF, bs_node BN, bs_node BN2\n         where \n         BDN.model_oid=PS.OID AND \n         BDA.BS_DESIGN_NODE_OID=BDN.OID AND\n         BAF.BS_DS_ATOM_OID=BDA.OID AND\n         BN.OID=BAF.BS_NODE_OID AND\n         BN2.OID=BN.parent_node\n  ) as surface\n  from PLS_ELEM PE ,PIPELINE_SEGMENT PS, SEGMENT S, V_CTRAY_PATTERN_LEVEL TR, NODE N1, NODE N2, ZONE Z, SYSTEMS SYS\n  where \n  PE.IDSQ = ${trayIdsq} AND\n  PE.TYPE=PS.TYPE AND PE.ZONE=PS.ZONE AND PE.SYSTEM=PS.SYSTEM AND PE.LINE=PS.LINE AND PE.PLS=PS.SQID AND\n  ((S.NODE1=PE.NODE1 AND S.NODE2=PE.NODE2) OR (S.NODE1=PE.NODE2 AND S.NODE2=PE.NODE1)) AND\n  S.PATTERN=TR.SEQID AND\n  PE.NODE1=N1.SEQID AND PE.NODE2=N2.SEQID AND\n  Z.SEQID=PE.ZONE AND\n  SYS.SEQID=PE.SYSTEM "
  }

  private def cableInTraySql(trayIdsq: String): String = {
    s"select code from cable where seqid in(\n\nselect CABLE from CAB_ROUTE  where (NODE1 in (\n\n    select NODE1 from PLS_ELEM where   IDSQ = ${trayIdsq}\n    union all\n    select NODE2 from PLS_ELEM where   IDSQ = ${trayIdsq}\n\n) AND NODE2 in \n    (\n    select NODE1 from PLS_ELEM where   IDSQ = ${trayIdsq}\n    union all\n    select NODE2 from PLS_ELEM where   IDSQ = ${trayIdsq}\n    )\n) \n) order by Code"
  }

  private def cablesInLineByTwoNodesSql(nodeName1: String, nodeName2: String): String = {
    s"select code from cable where seqid in(\n\nselect CABLE from CAB_ROUTE  where (NODE1 in (\n\n    select seqid from node where userid in('${nodeName1}', '${nodeName2}')\n    union all\n    select seqid from node where userid in('${nodeName1}', '${nodeName2}')\n) AND NODE2 in \n    (\n    select seqid from node where userid in('${nodeName1}', '${nodeName2}')\n    union all\n    select seqid from node where userid in('${nodeName1}', '${nodeName2}')\n    )\n) \n) order by Code"
  }

  private def traySqlByZonesAndSystems(zoneNames: String, systemNames: String): String = {
    s"select   \n  PE.IDSQ,\n  PS.OID as FDS_MODEL,\n  Z.NAME  as ZONE,\n  SYS.NAME  as SYSTEM,\n  PE.LINE,\n  PE.PLS,\n  PE.ELEM,\n  PE.WEIGHT,\n  PE.X_COG,\n  PE.Y_COG,\n  PE.Z_COG,\n  PE.CTYPE,\n  PE.TYPE,\n  N1.USERID  as NODE1,\n   " +
      s"N2.USERID  as NODE2,\n  PE.TRAY_LEVEL,\n  TR.STOCK_CODE,\n  N1.X *1000 as N1X,\n  N1.Y *1000 as N1Y,\n  N1.Z *1000 as N1Z,\n  N2.X *1000 as N2X,\n  N2.Y *1000 as N2Y,\n  N2.Z *1000 as N2Z,\n  " +
      s"SQRT( (N2.X-N1.X)*(N2.X-N1.X) + (N2.Y-N1.Y)*(N2.Y-N1.Y) + (N2.Z-N1.Z)*(N2.Z-N1.Z) )*1000 as LEN,\n  (\n         select \n         BN2.name\n         from BS_DESIGN_NODE  BDN, BS_DESIGN_ATOM BDA, " +
      s"BS_ATOM_FIXED_ATTRIBUTE BAF, bs_node BN, bs_node BN2\n         where \n         BDN.model_oid=PS.OID AND \n         BDA.BS_DESIGN_NODE_OID=BDN.OID AND\n         BAF.BS_DS_ATOM_OID=BDA.OID AND\n         " +
      s"BN.OID=BAF.BS_NODE_OID AND\n         BN2.OID=BN.parent_node\n  ) as surface ,TR.DESCR AS TRAYDESCR\n  from PLS_ELEM PE ,PIPELINE_SEGMENT PS, SEGMENT S, V_CTRAY_PATTERN_LEVEL TR, NODE N1, NODE N2, ZONE Z, SYSTEMS SYS\n  " +
      s"where \n   PE.TYPE=PS.TYPE AND PE.ZONE=PS.ZONE AND PE.SYSTEM=PS.SYSTEM AND PE.LINE=PS.LINE AND PE.PLS=PS.SQID AND\n  ((S.NODE1=PE.NODE1 AND S.NODE2=PE.NODE2) OR (S.NODE1=PE.NODE2 AND S.NODE2=PE.NODE1)) AND\n  " +
      s"S.PATTERN=TR.SEQID AND\n  PE.NODE1=N1.SEQID AND PE.NODE2=N2.SEQID AND\n  Z.SEQID=PE.ZONE AND\n  SYS.SEQID=PE.SYSTEM \n  AND Z.NAME in (${zoneNames}) and\n  SYS.NAME in (${systemNames})"
  }



  private def traySqlByZoneNames(zoneNames: String): String = {
    s"select   \n  PE.IDSQ,\n  PS.OID as FDS_MODEL,\n  Z.NAME  as ZONE,\n  SYS.NAME  as SYSTEM,\n  PE.LINE,\n  PE.PLS,\n  PE.ELEM,\n  PE.WEIGHT,\n  PE.X_COG,\n  PE.Y_COG,\n  PE.Z_COG,\n  PE.CTYPE,\n  PE.TYPE,\n  N1.USERID  as NODE1,\n   " +
      s"N2.USERID  as NODE2,\n  PE.TRAY_LEVEL,\n  TR.STOCK_CODE,\n  N1.X *1000 as N1X,\n  N1.Y *1000 as N1Y,\n  N1.Z *1000 as N1Z,\n  N2.X *1000 as N2X,\n  N2.Y *1000 as N2Y,\n  N2.Z *1000 as N2Z,\n  " +
      s"SQRT( (N2.X-N1.X)*(N2.X-N1.X) + (N2.Y-N1.Y)*(N2.Y-N1.Y) + (N2.Z-N1.Z)*(N2.Z-N1.Z) )*1000 as LEN,\n  (\n         select \n         BN2.name\n         from BS_DESIGN_NODE  BDN, BS_DESIGN_ATOM BDA, " +
      s"BS_ATOM_FIXED_ATTRIBUTE BAF, bs_node BN, bs_node BN2\n         where \n         BDN.model_oid=PS.OID AND \n         BDA.BS_DESIGN_NODE_OID=BDN.OID AND\n         BAF.BS_DS_ATOM_OID=BDA.OID AND\n         " +
      s"BN.OID=BAF.BS_NODE_OID AND\n         BN2.OID=BN.parent_node\n  ) as surface ,TR.DESCR AS TRAYDESCR\n  from PLS_ELEM PE ,PIPELINE_SEGMENT PS, SEGMENT S, V_CTRAY_PATTERN_LEVEL TR, NODE N1, NODE N2, ZONE Z, SYSTEMS SYS\n  " +
      s"where \n   PE.TYPE=PS.TYPE AND PE.ZONE=PS.ZONE AND PE.SYSTEM=PS.SYSTEM AND PE.LINE=PS.LINE AND PE.PLS=PS.SQID AND\n  ((S.NODE1=PE.NODE1 AND S.NODE2=PE.NODE2) OR (S.NODE1=PE.NODE2 AND S.NODE2=PE.NODE1)) AND\n  " +
      s"S.PATTERN=TR.SEQID AND\n  PE.NODE1=N1.SEQID AND PE.NODE2=N2.SEQID AND\n  Z.SEQID=PE.ZONE AND\n  SYS.SEQID=PE.SYSTEM \n  AND Z.NAME in (${zoneNames})"
  }

  private def traySqlBySystemNames(systemNames: String): String = {
    s"select   \n  PE.IDSQ,\n  PS.OID as FDS_MODEL,\n  Z.NAME  as ZONE,\n  SYS.NAME  as SYSTEM,\n  PE.LINE,\n  PE.PLS,\n  PE.ELEM,\n  PE.WEIGHT,\n  PE.X_COG,\n  PE.Y_COG,\n  PE.Z_COG,\n  PE.CTYPE,\n  PE.TYPE,\n  N1.USERID  as NODE1,\n   " +
      s"N2.USERID  as NODE2,\n  PE.TRAY_LEVEL,\n  TR.STOCK_CODE,\n  N1.X *1000 as N1X,\n  N1.Y *1000 as N1Y,\n  N1.Z *1000 as N1Z,\n  N2.X *1000 as N2X,\n  N2.Y *1000 as N2Y,\n  N2.Z *1000 as N2Z,\n  " +
      s"SQRT( (N2.X-N1.X)*(N2.X-N1.X) + (N2.Y-N1.Y)*(N2.Y-N1.Y) + (N2.Z-N1.Z)*(N2.Z-N1.Z) )*1000 as LEN,\n  (\n         select \n         BN2.name\n         from BS_DESIGN_NODE  BDN, BS_DESIGN_ATOM BDA, " +
      s"BS_ATOM_FIXED_ATTRIBUTE BAF, bs_node BN, bs_node BN2\n         where \n         BDN.model_oid=PS.OID AND \n         BDA.BS_DESIGN_NODE_OID=BDN.OID AND\n         BAF.BS_DS_ATOM_OID=BDA.OID AND\n         " +
      s"BN.OID=BAF.BS_NODE_OID AND\n         BN2.OID=BN.parent_node\n  ) as surface ,TR.DESCR AS TRAYDESCR\n  from PLS_ELEM PE ,PIPELINE_SEGMENT PS, SEGMENT S, V_CTRAY_PATTERN_LEVEL TR, NODE N1, NODE N2, ZONE Z, SYSTEMS SYS\n  " +
      s"where \n   PE.TYPE=PS.TYPE AND PE.ZONE=PS.ZONE AND PE.SYSTEM=PS.SYSTEM AND PE.LINE=PS.LINE AND PE.PLS=PS.SQID AND\n  ((S.NODE1=PE.NODE1 AND S.NODE2=PE.NODE2) OR (S.NODE1=PE.NODE2 AND S.NODE2=PE.NODE1)) AND\n  " +
      s"S.PATTERN=TR.SEQID AND\n  PE.NODE1=N1.SEQID AND PE.NODE2=N2.SEQID AND\n  Z.SEQID=PE.ZONE AND\n  SYS.SEQID=PE.SYSTEM \n  and\n  SYS.NAME in (${systemNames})"
  }

/*  private def allTraysSql(): String = {
    s"select STOCK_CODE, sum(WEIGHT) as WEIGHT, sum(LEN) as LEN\nfrom\n    (\n    select   \n              TR.STOCK_CODE,\n              PE.WEIGHT,\n              " +
      s"SQRT( (N2.X-N1.X)*(N2.X-N1.X) + (N2.Y-N1.Y)*(N2.Y-N1.Y) + (N2.Z-N1.Z)*(N2.Z-N1.Z) )*1000 as LEN\n              from PLS_ELEM PE ,PIPELINE_SEGMENT PS, SEGMENT S, V_CTRAY_PATTERN_LEVEL TR, NODE N1, NODE N2, ZONE Z, SYSTEMS SYS\n   " +
      s"where \n              PE.TYPE=PS.TYPE AND PE.ZONE=PS.ZONE AND PE.SYSTEM=PS.SYSTEM AND PE.LINE=PS.LINE AND PE.PLS=PS.SQID AND\n              ((S.NODE1=PE.NODE1 AND S.NODE2=PE.NODE2) OR (S.NODE1=PE.NODE2 AND S.NODE2=PE.NODE1)) AND\n  " +
      s"S.PATTERN=TR.SEQID AND\n              PE.NODE1=N1.SEQID AND PE.NODE2=N2.SEQID AND\n              Z.SEQID=PE.ZONE AND\n              SYS.SEQID=PE.SYSTEM   ) \n  group by STOCK_CODE"
  }*/
  private def allTraysSql(): String = {
    s"select   \n  PE.IDSQ,\n  PS.OID as FDS_MODEL,\n  Z.NAME  as ZONE,\n  SYS.NAME  as SYSTEM,\n  PE.LINE,\n  PE.PLS,\n  PE.ELEM,\n  PE.WEIGHT,\n  PE.X_COG,\n  PE.Y_COG,\n  PE.Z_COG,\n  PE.CTYPE,\n  PE.TYPE,\n  N1.USERID  as NODE1,\n   " +
      s"N2.USERID  as NODE2,\n  PE.TRAY_LEVEL,\n  TR.STOCK_CODE,\n  N1.X *1000 as N1X,\n  N1.Y *1000 as N1Y,\n  N1.Z *1000 as N1Z,\n  N2.X *1000 as N2X,\n  N2.Y *1000 as N2Y,\n  N2.Z *1000 as N2Z,\n  " +
      s"SQRT( (N2.X-N1.X)*(N2.X-N1.X) + (N2.Y-N1.Y)*(N2.Y-N1.Y) + (N2.Z-N1.Z)*(N2.Z-N1.Z) )*1000 as LEN,\n  (\n         select \n         BN2.name\n         from BS_DESIGN_NODE  BDN, BS_DESIGN_ATOM BDA, " +
      s"BS_ATOM_FIXED_ATTRIBUTE BAF, bs_node BN, bs_node BN2\n         where \n         BDN.model_oid=PS.OID AND \n         BDA.BS_DESIGN_NODE_OID=BDN.OID AND\n         BAF.BS_DS_ATOM_OID=BDA.OID AND\n         " +
      s"BN.OID=BAF.BS_NODE_OID AND\n         BN2.OID=BN.parent_node\n  ) as surface ,TR.DESCR AS TRAYDESCR\n  from PLS_ELEM PE ,PIPELINE_SEGMENT PS, SEGMENT S, V_CTRAY_PATTERN_LEVEL TR, NODE N1, NODE N2, ZONE Z, SYSTEMS SYS\n  " +
      s"where \n   PE.TYPE=PS.TYPE AND PE.ZONE=PS.ZONE AND PE.SYSTEM=PS.SYSTEM AND PE.LINE=PS.LINE AND PE.PLS=PS.SQID AND\n  ((S.NODE1=PE.NODE1 AND S.NODE2=PE.NODE2) OR (S.NODE1=PE.NODE2 AND S.NODE2=PE.NODE1)) AND\n  " +
      s"S.PATTERN=TR.SEQID AND\n  PE.NODE1=N1.SEQID AND PE.NODE2=N2.SEQID AND\n  Z.SEQID=PE.ZONE AND\n  SYS.SEQID=PE.SYSTEM"
  }
  private def cbxAll()=s"select  \nPE.IDSQ, \n(select USERID from ELEMENT where UUID=PE.UUID) as USERID,\nZ.NAME  as ZONE,\nSYS.NAME  as SYSTEM,\nPE.X_COG, \nPE.Y_COG, \nPE.Z_COG,\nPE.WEIGHT, \nPE.NODE1, \nPE.NODE2, \nPL.TYPE, \nPL.SEAL_TYPE, \nPL.CODE, \nPL.DESCR, \nPL.STOCK_CODE, \n(select userid from pntr_list where FITT_OID=PE.IDSQ) as PENRTRATION \nfrom PLS_ELEM PE, V_CABLE_PENETRATION_LIBRARY PL , ZONE Z, SYSTEMS SYS\nwhere  \nPE.TRAY_FITTING=PL.OID AND \nZ.SEQID=PE.ZONE AND\nSYS.SEQID=PE.SYSTEM  AND\n(PL.SEAL_TYPE='S' OR PL.SEAL_TYPE is null) AND  PL.TYPE=1  \nAND CODE not LIKE 'Обделка%'"
  private def cbxByZones(zoneNames: String)=s"select  \nPE.IDSQ, \n(select USERID from ELEMENT where UUID=PE.UUID) as USERID,\nZ.NAME  as ZONE,\nSYS.NAME  as SYSTEM,\nPE.X_COG, \nPE.Y_COG, \nPE.Z_COG,\nPE.WEIGHT, \nPE.NODE1, \nPE.NODE2, \nPL.TYPE, \nPL.SEAL_TYPE, \nPL.CODE, \nPL.DESCR, \nPL.STOCK_CODE, \n(select userid from pntr_list where FITT_OID=PE.IDSQ) as PENRTRATION \nfrom PLS_ELEM PE, V_CABLE_PENETRATION_LIBRARY PL , ZONE Z, SYSTEMS SYS\nwhere  \nPE.TRAY_FITTING=PL.OID AND \nZ.SEQID=PE.ZONE AND\nSYS.SEQID=PE.SYSTEM  AND\n(PL.SEAL_TYPE='S' OR PL.SEAL_TYPE is null) AND  PL.TYPE=1  \nAND CODE not LIKE 'Обделка%'AND Z.NAME in (${zoneNames})"
  private def cbxBySystems(systemNames: String)=s"select  \nPE.IDSQ, \n(select USERID from ELEMENT where UUID=PE.UUID) as USERID,\nZ.NAME  as ZONE,\nSYS.NAME  as SYSTEM,\nPE.X_COG, \nPE.Y_COG, \nPE.Z_COG,\nPE.WEIGHT, \nPE.NODE1, \nPE.NODE2, \nPL.TYPE, \nPL.SEAL_TYPE, \nPL.CODE, \nPL.DESCR, \nPL.STOCK_CODE, \n(select userid from pntr_list where FITT_OID=PE.IDSQ) as PENRTRATION \nfrom PLS_ELEM PE, V_CABLE_PENETRATION_LIBRARY PL , ZONE Z, SYSTEMS SYS\nwhere  \nPE.TRAY_FITTING=PL.OID AND \nZ.SEQID=PE.ZONE AND\nSYS.SEQID=PE.SYSTEM  AND\n(PL.SEAL_TYPE='S' OR PL.SEAL_TYPE is null) AND  PL.TYPE=1  \nAND CODE not LIKE 'Обделка%' AND SYS.NAME in (${systemNames})"
  private def cbxByZonesAndSystems(zoneNames: String, systemNames: String)=s"select  \nPE.IDSQ, \n(select USERID from ELEMENT where UUID=PE.UUID) as USERID,\nZ.NAME  as ZONE,\nSYS.NAME  as SYSTEM,\nPE.X_COG, \nPE.Y_COG, \nPE.Z_COG,\nPE.WEIGHT, \nPE.NODE1, \nPE.NODE2, \nPL.TYPE, \nPL.SEAL_TYPE, \nPL.CODE, \nPL.DESCR, \nPL.STOCK_CODE, \n(select userid from pntr_list where FITT_OID=PE.IDSQ) as PENRTRATION \nfrom PLS_ELEM PE, V_CABLE_PENETRATION_LIBRARY PL , ZONE Z, SYSTEMS SYS\nwhere  \nPE.TRAY_FITTING=PL.OID AND \nZ.SEQID=PE.ZONE AND\nSYS.SEQID=PE.SYSTEM  AND\n(PL.SEAL_TYPE='S' OR PL.SEAL_TYPE is null) AND  PL.TYPE=1  \nAND CODE not LIKE 'Обделка%'AND Z.NAME in (${zoneNames}) AND SYS.NAME in (${systemNames})"

  private val duration: FiniteDuration = Duration(2, SECONDS)

  private def collectionTrayMountData(): MongoCollection[TrayMountData] = mongoDatabase().getCollection("eleTrayMountData")

  private def collectioneleTrayMountRules(): MongoCollection[TrayMountRules] = mongoDatabase().getCollection("eleTrayMountRules")

  def TrayBySeqId(project: String, trayIdSeq: String): ForanTray = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = traySQL(trayIdSeq)
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = {
            if (rs.next()) {
              val marign: Int = calculateH(Option[Double](rs.getDouble("X_COG")).getOrElse(0),
                Option[Double](rs.getDouble("Y_COG")).getOrElse(0),
                Option[Double](rs.getDouble("Z_COG")).getOrElse(0), Option[String](rs.getString("SURFACE")).getOrElse(""))
              val materialId = getMaterialFromString(Option[String](rs.getString("SURFACE")).getOrElse(""))
              ForanTray(
                Option[Int](rs.getInt("IDSQ")).getOrElse(0),
                Option[Int](rs.getInt("FDS_MODEL")).getOrElse(0),
                Option[String](rs.getString("ZONE")).getOrElse(""),
                Option[String](rs.getString("SYSTEM")).getOrElse(""),
                Option[Int](rs.getInt("LINE")).getOrElse(0),
                Option[Int](rs.getInt("PLS")).getOrElse(0),
                Option[Int](rs.getInt("ELEM")).getOrElse(0),
                Option[Double](rs.getDouble("WEIGHT")).getOrElse(0),
                Option[Double](rs.getDouble("X_COG")).getOrElse(0),
                Option[Double](rs.getDouble("Y_COG")).getOrElse(0),
                Option[Double](rs.getDouble("Z_COG")).getOrElse(0),
                Option[String](rs.getString("CTYPE")).getOrElse(""),
                Option[Int](rs.getInt("TYPE")).getOrElse(0),
                Option[String](rs.getString("NODE1")).getOrElse(""),
                Option[String](rs.getString("NODE2")).getOrElse(""),
                Option[Int](rs.getInt("TRAY_LEVEL")).getOrElse(0),
                Option[String](rs.getString("STOCK_CODE")).getOrElse(""),
                Option[Double](rs.getDouble("N1X")).getOrElse(0),
                Option[Double](rs.getDouble("N1Y")).getOrElse(0),
                Option[Double](rs.getDouble("N1Z")).getOrElse(0),
                Option[Double](rs.getDouble("N2X")).getOrElse(0),
                Option[Double](rs.getDouble("N2Y")).getOrElse(0),
                Option[Double](rs.getDouble("N2Z")).getOrElse(0),
                Option[Double](rs.getDouble("LEN")).getOrElse(0),
                Option[String](rs.getString("SURFACE")).getOrElse(""),
                Option[String](rs.getString("TRAYDESCR")).getOrElse(""),
                marign,
                materialId
              )
            } else {
              ForanTray()
            }
          }
          stmt.close()
          connection.close()
          ret
        }
        catch {
          case _: Throwable => ForanTray()
        }
      }
      case None => ForanTray()
    }
  }

  def retrieveTraysByZoneNameAndSysName(project: String, zones: List[String], systems: List[String]): List[ForanTray] = {

    val sql: String = (zones.isEmpty, systems.isEmpty) match {
      case (true, true) => allTraysSql()
      case (false, true) => traySqlByZoneNames(listToSqlString(zones))
      case (true, false) => traySqlBySystemNames(listToSqlString(systems))
      case _ => traySqlByZonesAndSystems(listToSqlString(zones), listToSqlString(systems))
    }
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[ForanTray]
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            val marign: Int = calculateH(Option[Double](rs.getDouble("X_COG")).getOrElse(0),
              Option[Double](rs.getDouble("Y_COG")).getOrElse(0),
              Option[Double](rs.getDouble("Z_COG")).getOrElse(0), Option[String](rs.getString("SURFACE")).getOrElse(""))
            val materialId: Int = getMaterialFromString(Option[String](rs.getString("SURFACE")).getOrElse(""))

            buffer += ForanTray(
              Option[Int](rs.getInt("IDSQ")).getOrElse(0),
              Option[Int](rs.getInt("FDS_MODEL")).getOrElse(0),
              Option[String](rs.getString("ZONE")).getOrElse(""),
              Option[String](rs.getString("SYSTEM")).getOrElse(""),
              Option[Int](rs.getInt("LINE")).getOrElse(0),
              Option[Int](rs.getInt("PLS")).getOrElse(0),
              Option[Int](rs.getInt("ELEM")).getOrElse(0),
              Option[Double](rs.getDouble("WEIGHT")).getOrElse(0),
              Option[Double](rs.getDouble("X_COG")).getOrElse(0),
              Option[Double](rs.getDouble("Y_COG")).getOrElse(0),
              Option[Double](rs.getDouble("Z_COG")).getOrElse(0),
              Option[String](rs.getString("CTYPE")).getOrElse(""),
              Option[Int](rs.getInt("TYPE")).getOrElse(0),
              Option[String](rs.getString("NODE1")).getOrElse(""),
              Option[String](rs.getString("NODE2")).getOrElse(""),
              Option[Int](rs.getInt("TRAY_LEVEL")).getOrElse(0),
              Option[String](rs.getString("STOCK_CODE")).getOrElse(""),
              Option[Double](rs.getDouble("N1X")).getOrElse(0),
              Option[Double](rs.getDouble("N1Y")).getOrElse(0),
              Option[Double](rs.getDouble("N1Z")).getOrElse(0),
              Option[Double](rs.getDouble("N2X")).getOrElse(0),
              Option[Double](rs.getDouble("N2Y")).getOrElse(0),
              Option[Double](rs.getDouble("N2Z")).getOrElse(0),
              Option[Double](rs.getDouble("LEN")).getOrElse(0),
              Option[String](rs.getString("SURFACE")).getOrElse(""),
              Option[String](rs.getString("TRAYDESCR")).getOrElse(""),
              marign,
              materialId
            )
          }
          rs.close()
          stmt.close()
          connection.commit()
          connection.close()
          buffer.toList
        }
        catch {
          case _: Throwable =>
            List.empty[ForanTray]
        }
      }
      case None => List.empty[ForanTray]
    }
  }


  def retrieveCBXByZoneNameAndSysName(project: String, zones: List[String], systems: List[String]): List[ForanCBX] = {

    val sql: String = (zones.isEmpty, systems.isEmpty) match {
      case (true, true) => cbxAll()
      case (false, true) => cbxByZones(listToSqlString(zones))
      case (true, false) => cbxBySystems(listToSqlString(systems))
      case _ => cbxByZonesAndSystems(listToSqlString(zones), listToSqlString(systems))
    }
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[ForanCBX]
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            buffer += ForanCBX(
              Option[Int](rs.getInt("IDSQ")).getOrElse(0),
              Option[String](rs.getString("ZONE")).getOrElse(""),
              Option[String](rs.getString("SYSTEM")).getOrElse(""),
              Option[Double](rs.getDouble("X_COG")).getOrElse(0),
              Option[Double](rs.getDouble("Y_COG")).getOrElse(0),
              Option[Double](rs.getDouble("Z_COG")).getOrElse(0),
              Option[Double](rs.getDouble("WEIGHT")).getOrElse(0),
              Option[String](rs.getString("NODE1")).getOrElse(""),
              Option[String](rs.getString("NODE2")).getOrElse(""),
              Option[String](rs.getString("CODE")).getOrElse(""),
              Option[String](rs.getString("DESCR")).getOrElse(""),
              Option[String](rs.getString("STOCK_CODE")).getOrElse(""),
              Option[String](rs.getString("PENRTRATION")).getOrElse("")
            )
          }
          rs.close()
          stmt.close()
          connection.commit()
          connection.close()
          buffer.toList
        }
        catch {
          case _: Throwable =>
            List.empty[ForanCBX]
        }
      }
      case None => List.empty[ForanCBX]
    }
  }





  def cablesByTraySeqId(project: String, trayIdSeq: String): List[String] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(cableInTraySql(trayIdSeq))
          val buffer = ListBuffer.empty[String]
          while (rs.next()) {
            buffer += Option[String](rs.getString("CODE")).getOrElse("NF")
          }
          stmt.close()
          connection.close()
          buffer.toList
        }
        catch {
          case _: Throwable => List.empty[String]
        }
      }
      case None => List.empty[String]
    }
  }

  def cablesinLineByTwoNodeNames(project: String, nodeName1: String, nodeName2: String): List[String] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(cablesInLineByTwoNodesSql(nodeName1, nodeName2))
          val buffer = ListBuffer.empty[String]
          while (rs.next()) {
            buffer += Option[String](rs.getString("CODE")).getOrElse("NF")
          }
          stmt.close()
          connection.close()
          buffer.toList
        }
        catch {
          case _: Throwable => List.empty[String]
        }
      }
      case None => List.empty[String]
    }
  }

  private def getMaterialFromString(in: String): Int = {
    if (in.nonEmpty && in.contains(";")) {
      if (in.split(";").length >= 2) {
        in.split(";")(1).toIntOption.getOrElse(3)
      } else {
        3
      }
    } else {
      3
    }
  }

  def retrieveTraysMountDate(): List[TrayMountData] = {
    val allelems = collectionTrayMountData().find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    allelems.value.get.getOrElse(ListBuffer.empty[TrayMountData].toSeq).toList
  }

  def retrieveTraysMountRules(): List[TrayMountRules] = {
    val allelems = collectioneleTrayMountRules().find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    allelems.value.get.getOrElse(Seq.empty[TrayMountRules]).toList
  }

  def retrieveAllTrayMountData(): List[TrayMountData] = {
    val allelems: Future[Seq[TrayMountData]] = collectionTrayMountData().find().toFuture()
    Await.result(allelems, duration)
    allelems.value.get.getOrElse(Seq.empty[TrayMountData]).toList
  }

  def retrieveTrayMountDataByTrm(trmCode: String, mountData: List[TrayMountData]): TrayMountData = {
    //val allelems: Future[Seq[TrayMountData]] = collectionTrayMountData().find(equal("trmCode", trmCode)).toFuture()
    //Await.result(allelems, duration)
    mountData.find(s => s.trmCode.equals(trmCode)) match {
      case Some(value) => value
      case None => TrayMountData()
    }
  }


}

