package local.ele.cl

import local.common.DBRequests.{findWorkshopMaterialContains, findWorkshopMaterialEquals, retrieveAllMaterialsByProject}
import local.domain.WorkShopMaterial
import local.ele.CommonEle.Cable
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

trait CableListHelper {

  def retrieveCablesByComplectSql(complectName: String): String = {
    val par: String = {
      val in = complectName.split("-")
      if (in.nonEmpty && in.length == 3) in(1) + "-" + in(2) else ""
    }
    val ret: String = s"select\nSEQID, \nCODE,\nSPEC,\nSECT,\nSECTION,\nMARK, \nSCHEME,\n (select userid from  node where seqid=FROM_N_ID) as from_n,\n\t(select code from  ROUT_AREA where seqid = (select r_area from  node where seqid=FROM_N_ID)) as from_n_room,\n\t(select x from  node where seqid=FROM_N_ID) * 1000 as from_n_x,\n\t(select y from  node where seqid=FROM_N_ID) * 1000 as from_n_y,\n\t(select z from  node where seqid=FROM_N_ID) * 1000 as from_n_z,\nFROM_E_USERID,    \nFROM_E_USERID_ELEC,                                                                                                   \nFROM_E_ROOM,   \nFROM_E_X,   \nFROM_E_Y,   \nFROM_E_Z,\nTO_E_USERID, \n\t(select userid from  node where seqid=TO_N_ID) as to_n,\n\t(select code from  ROUT_AREA where seqid = (select r_area from  node where seqid=TO_N_ID)) as to_n_room,\n\t(select x from  node where seqid=TO_N_ID) * 1000 as to_n_x,\n\t(select y from  node where seqid=TO_N_ID) * 1000 as to_n_y,\n\t(select z from  node where seqid=TO_N_ID) * 1000 as to_n_z,                                                  \nTO_E_USERID_ELEC,                         \nTO_E_ROOM,     \nTO_E_X,     \nTO_E_Y,\nTO_E_Z,\nLEN,                  \nCABLEPATH,                                                                                                                                                   \nADDDATA,                                                                                                                                                     \nPARTNUMBER ,        \nSTOCKCODE\nfrom\n(\nselect \nC.seqid,\nC.code,\nC.spec,\nC.sect,\n(select section from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as section,\n(select cab_spec from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as mark,\n(select descr from  systems_lang where system=syst and lang = -2) as scheme,\n(select node1 from cab_route  where cable= C.seqid and seq_pos=(select min(seq_pos) from CAB_ROUTE where cable= C.seqid)) as FROM_N_ID,\n(select node2 from cab_route  where cable= C.seqid and seq_pos=(select max(seq_pos) from CAB_ROUTE where cable= C.seqid)) as TO_N_ID,\n\n(select userid from  v_element where oid=C.from_e) as from_e_userid,\n(select DESCR1 from  ELEMENT_ELEC where ELEM=C.from_e) as from_e_userid_elec,\n(select userid from  zone where oid=(select zone from  element where oid=C.from_e)) as from_e_room,\n(select xpos from  v_element where oid=C.from_e) as from_e_x,\n(select ypos from  v_element where oid=C.from_e) as from_e_y,\n(select zpos from  v_element where oid=C.from_e) as from_e_z,\n\n(select userid from  v_element where oid=C.to_e) as to_e_userid,\n(select DESCR1 from  ELEMENT_ELEC where ELEM=C.to_e) as to_e_userid_elec,\n(select userid from  zone where oid=(select zone from  element where oid=C.to_e)) as to_e_room,\n(select xpos from  v_element where oid=C.to_e) as to_e_x,\n(select ypos from  v_element where oid=C.to_e) as to_e_y,\n(select zpos from  v_element where oid=C.to_e) as to_e_z,\nget_cab_length (C.SEQID,C.FROM_E,C.TO_E,C.F_ROUT,C.PERC_CORR,C.EXT_LEN_1,C.EXT_LEN_2,C.EST_LENG) as len,\nget_cab_route_area(C.seqid) as cablepath,\n(select ADDITIONAL_DATA from  CAB_SPEC where SEQID=C.spec) as adddata,\n(select PARTNUMBER from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as partnumber,\nss.CODENUMBER  as stockcode\n\n\nfrom cable C, SECTION_SPEC SS\n\nwhere \nseqid in(\n        select distinct cable from(\n        select cable, node1 as node from CAB_ROUTE\n        union all\n        select cable, node2 as node from CAB_ROUTE \n        ) where node in (select seqid from NODE where r_area in \n        (select seqid from ROUT_AREA where descr='${par}'))\n) AND\n(C.spec=SS.spec and SS.nom_sect=C.sect)\n)\n--where \n--REGEXP_INSTR (CABLEPATH, '\\s\\d{6}\\s') = 0\n--ROWNUM <= 10\norder by code\n\n "
    ret
  }

  def retrieveCablesByComplectMagistralVariantSQL(complectName: String): String = {
    val par: String = {
      val in = complectName.split("-")
      if (in.nonEmpty && in.length == 3) in(1) + "-" + in(2) else ""
    }
    val ret = s"select\nSEQID, \nCODE,\nSPEC,\nSECT,\nSECTION,\nMARK, \nSCHEME,\nfrom_n,\nfrom_n_room,\nfrom_n_x,\nfrom_n_y,\nfrom_n_z,\nFROM_E_USERID,    \nFROM_E_USERID_ELEC,  \nFROM_E_ROOM,   \nFROM_E_X,   \nFROM_E_Y,   \nFROM_E_Z,\nTO_E_USERID,\nto_n, \nto_n_room,\nto_n_x,\nto_n_y,\nto_n_z,\nTO_E_USERID_ELEC,  \nTO_E_ROOM,     \nTO_E_X,     \nTO_E_Y,\nTO_E_Z,\nLEN, \nCABLEPATH, \nADDDATA,   \nPARTNUMBER , \nSTOCKCODE\nfrom(\n\nselect\nSEQID, \nCODE,\nSPEC,\nSECT,\nSECTION,\nMARK, \nSCHEME,\n\t(select userid from  node where seqid=FROM_N_ID) as from_n,\n\t(select code from  ROUT_AREA where seqid = (select r_area from  node where seqid=FROM_N_ID)) as from_n_room,\n\t(select x from  node where seqid=FROM_N_ID) * 1000 as from_n_x,\n\t(select y from  node where seqid=FROM_N_ID) * 1000 as from_n_y,\n\t(select z from  node where seqid=FROM_N_ID) * 1000 as from_n_z,\nFROM_E_USERID,    \nFROM_E_USERID_ELEC,  \nFROM_E_ROOM,   \nFROM_E_X,   \nFROM_E_Y,   \nFROM_E_Z,\nTO_E_USERID, \n(select userid from  node where seqid=TO_N_ID) as to_n,\n(select code from  ROUT_AREA where seqid = (select r_area from  node where seqid=TO_N_ID)) as to_n_room,\n(select x from  node where seqid=TO_N_ID) * 1000 as to_n_x,\n(select y from  node where seqid=TO_N_ID) * 1000 as to_n_y,\n(select z from  node where seqid=TO_N_ID) * 1000 as to_n_z,  \nTO_E_USERID_ELEC,  \nTO_E_ROOM,     \nTO_E_X,     \nTO_E_Y,\nTO_E_Z,\nLEN,                  \nCABLEPATH,                                                                                                                                                   \nADDDATA, \nPARTNUMBER ,  \nSTOCKCODE\nfrom\n(\nselect \nC.seqid,\nC.code,\nC.spec,\nC.sect,\n(select section from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as section,\n(select cab_spec from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as mark,\n(select descr from  systems_lang where system=syst and lang = -2) as scheme,\n(select node1 from cab_route  where cable= C.seqid and seq_pos=(select min(seq_pos) from CAB_ROUTE where cable= C.seqid)) as FROM_N_ID,\n(select node2 from cab_route  where cable= C.seqid and seq_pos=(select max(seq_pos) from CAB_ROUTE where cable= C.seqid)) as TO_N_ID,\n\n(select userid from  v_element where oid=C.from_e) as from_e_userid,\n(select DESCR1 from  ELEMENT_ELEC where ELEM=C.from_e) as from_e_userid_elec,\n(select userid from  zone where oid=(select zone from  element where oid=C.from_e)) as from_e_room,\n(select xpos from  v_element where oid=C.from_e) as from_e_x,\n(select ypos from  v_element where oid=C.from_e) as from_e_y,\n(select zpos from  v_element where oid=C.from_e) as from_e_z,\n\n(select userid from  v_element where oid=C.to_e) as to_e_userid,\n(select DESCR1 from  ELEMENT_ELEC where ELEM=C.to_e) as to_e_userid_elec,\n(select userid from  zone where oid=(select zone from  element where oid=C.to_e)) as to_e_room,\n(select xpos from  v_element where oid=C.to_e) as to_e_x,\n(select ypos from  v_element where oid=C.to_e) as to_e_y,\n(select zpos from  v_element where oid=C.to_e) as to_e_z,\nget_cab_length (C.SEQID,C.FROM_E,C.TO_E,C.F_ROUT,C.PERC_CORR,C.EXT_LEN_1,C.EXT_LEN_2,C.EST_LENG) as len,\nget_cab_route_area(C.seqid) as cablepath,\n(select ADDITIONAL_DATA from  CAB_SPEC where SEQID=C.spec) as adddata,\n(select PARTNUMBER from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as partnumber,\nss.CODENUMBER  as stockcode\n\n\nfrom cable C, SECTION_SPEC SS\n\nwhere \nseqid in(\n        select distinct cable from(\n        select cable, node1 as node from CAB_ROUTE\n        union all\n        select cable, node2 as node from CAB_ROUTE \n        ) where node in (select seqid from NODE where r_area in \n        (select seqid from ROUT_AREA where descr<>'${par}'))\n) AND\n(C.spec=SS.spec and SS.nom_sect=C.sect)\n)\nwhere REGEXP_INSTR (CABLEPATH, '\\s\\d{6}\\s') >0\n\nunion all\n\nselect\nSEQID, \nCODE,\nSPEC,\nSECT,\nSECTION,\nMARK, \nSCHEME,                                                                                                                                                 \n\t(select userid from  node where seqid=FROM_N_ID) as from_n,\n\t(select code from  ROUT_AREA where seqid = (select r_area from  node where seqid=FROM_N_ID)) as from_n_room,\n\t(select x from  node where seqid=FROM_N_ID) * 1000 as from_n_x,\n\t(select y from  node where seqid=FROM_N_ID) * 1000 as from_n_y,\n\t(select z from  node where seqid=FROM_N_ID) * 1000 as from_n_z,\nFROM_E_USERID,    \nFROM_E_USERID_ELEC,                                                                                                   \nFROM_E_ROOM,   \nFROM_E_X,   \nFROM_E_Y,   \nFROM_E_Z,\nTO_E_USERID, \n\t(select userid from  node where seqid=TO_N_ID) as to_n,\n\t(select code from  ROUT_AREA where seqid = (select r_area from  node where seqid=TO_N_ID)) as to_n_room,\n\t(select x from  node where seqid=TO_N_ID) * 1000 as to_n_x,\n\t(select y from  node where seqid=TO_N_ID) * 1000 as to_n_y,\n\t(select z from  node where seqid=TO_N_ID) * 1000 as to_n_z,                                                  \nTO_E_USERID_ELEC,                         \nTO_E_ROOM,     \nTO_E_X,     \nTO_E_Y,\nTO_E_Z,\nLEN,                  \nCABLEPATH, \nADDDATA,   \nPARTNUMBER , \nSTOCKCODE\nfrom\n(\nselect \nC.seqid,\nC.code,\nC.spec,\nC.sect,\n(select section from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as section,\n(select cab_spec from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as mark,\n(select descr from  systems_lang where system=syst and lang = -2) as scheme,\n(select node1 from cab_route  where cable= C.seqid and seq_pos=(select min(seq_pos) from CAB_ROUTE where cable= C.seqid)) as FROM_N_ID,\n(select node2 from cab_route  where cable= C.seqid and seq_pos=(select max(seq_pos) from CAB_ROUTE where cable= C.seqid)) as TO_N_ID,\n\n(select userid from  v_element where oid=C.from_e) as from_e_userid,\n(select DESCR1 from  ELEMENT_ELEC where ELEM=C.from_e) as from_e_userid_elec,\n(select userid from  zone where oid=(select zone from  element where oid=C.from_e)) as from_e_room,\n(select xpos from  v_element where oid=C.from_e) as from_e_x,\n(select ypos from  v_element where oid=C.from_e) as from_e_y,\n(select zpos from  v_element where oid=C.from_e) as from_e_z,\n\n(select userid from  v_element where oid=C.to_e) as to_e_userid,\n(select DESCR1 from  ELEMENT_ELEC where ELEM=C.to_e) as to_e_userid_elec,\n(select userid from  zone where oid=(select zone from  element where oid=C.to_e)) as to_e_room,\n(select xpos from  v_element where oid=C.to_e) as to_e_x,\n(select ypos from  v_element where oid=C.to_e) as to_e_y,\n(select zpos from  v_element where oid=C.to_e) as to_e_z,\nget_cab_length (C.SEQID,C.FROM_E,C.TO_E,C.F_ROUT,C.PERC_CORR,C.EXT_LEN_1,C.EXT_LEN_2,C.EST_LENG) as len,\nget_cab_route_area(C.seqid) as cablepath,\n(select ADDITIONAL_DATA from  CAB_SPEC where SEQID=C.spec) as adddata,\n(select PARTNUMBER from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as partnumber,\nss.CODENUMBER  as stockcode\n\n\nfrom cable C, SECTION_SPEC SS\n\nwhere \nseqid in(\n        select distinct cable from(\n        select cable, node1 as node from CAB_ROUTE\n        union all\n        select cable, node2 as node from CAB_ROUTE \n        ) where node in (select seqid from NODE where r_area in \n        (select seqid from ROUT_AREA where descr='${par}'))\n) AND\n(C.spec=SS.spec and SS.nom_sect=C.sect)\n)\n \n)\norder by code\n\n\n"
    ret
  }


  def retrieveCablesByRoomSql(room: String) = s"select\nSEQID, \nCODE,\nSPEC,\nSECT,\nSECTION,\nMARK, \nSCHEME,  \nfrom_n,\nfrom_n_room,\nfrom_n_x,\nfrom_n_y,\nfrom_n_z,\nFROM_E_USERID,    \nFROM_E_USERID_ELEC, \nFROM_E_ROOM,   \nFROM_E_X,   \nFROM_E_Y,   \nFROM_E_Z,\nTO_E_USERID, \nto_n,\nto_n_room,\nto_n_x,\nto_n_y,\nto_n_z, \nTO_E_USERID_ELEC, \nTO_E_ROOM,     \nTO_E_X,     \nTO_E_Y,\nTO_E_Z,\nLEN,  \nCABLEPATH, \nADDDATA, \nPARTNUMBER ,  \nSTOCKCODE\nfrom\n(select\nSEQID, \nCODE,\nSPEC,\nSECT,\nSECTION,\nMARK, \nSCHEME, \n(select userid from  node where seqid=FROM_N_ID) as from_n,\n(select code from  ROUT_AREA where seqid = (select r_area from  node where seqid=FROM_N_ID)) as from_n_room,\n(select x from  node where seqid=FROM_N_ID) * 1000 as from_n_x,\n(select y from  node where seqid=FROM_N_ID) * 1000 as from_n_y,\n(select z from  node where seqid=FROM_N_ID) * 1000 as from_n_z,\nFROM_E_USERID,    \nFROM_E_USERID_ELEC,  \nFROM_E_ROOM,   \nFROM_E_X,   \nFROM_E_Y,   \nFROM_E_Z,\nTO_E_USERID, \n(select userid from  node where seqid=TO_N_ID) as to_n,\n(select code from  ROUT_AREA where seqid = (select r_area from  node where seqid=TO_N_ID)) as to_n_room,\n(select x from  node where seqid=TO_N_ID) * 1000 as to_n_x,\n(select y from  node where seqid=TO_N_ID) * 1000 as to_n_y,\n(select z from  node where seqid=TO_N_ID) * 1000 as to_n_z,  \nTO_E_USERID_ELEC, \nTO_E_ROOM,     \nTO_E_X,     \nTO_E_Y,\nTO_E_Z,\nLEN,  \nCABLEPATH,   \nADDDATA, \nPARTNUMBER ,  \nSTOCKCODE\nfrom\n(\nselect \nC.seqid,\nC.code,\nC.spec,\nC.sect,\n(select section from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as section,\n(select cab_spec from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as mark,\n(select descr from  systems_lang where system=syst and lang = -2) as scheme,\n(select node1 from cab_route  where cable= C.seqid and seq_pos=(select min(seq_pos) from CAB_ROUTE where cable= C.seqid)) as FROM_N_ID,\n(select node2 from cab_route  where cable= C.seqid and seq_pos=(select max(seq_pos) from CAB_ROUTE where cable= C.seqid)) as TO_N_ID,\n(select userid from  v_element where oid=C.from_e) as from_e_userid,\n(select DESCR1 from  ELEMENT_ELEC where ELEM=C.from_e) as from_e_userid_elec,\n(select userid from  zone where oid=(select zone from  element where oid=C.from_e)) as from_e_room,\n(select xpos from  v_element where oid=C.from_e) as from_e_x,\n(select ypos from  v_element where oid=C.from_e) as from_e_y,\n(select zpos from  v_element where oid=C.from_e) as from_e_z,\n(select userid from  v_element where oid=C.to_e) as to_e_userid,\n(select DESCR1 from  ELEMENT_ELEC where ELEM=C.to_e) as to_e_userid_elec,\n(select userid from  zone where oid=(select zone from  element where oid=C.to_e)) as to_e_room,\n(select xpos from  v_element where oid=C.to_e) as to_e_x,\n(select ypos from  v_element where oid=C.to_e) as to_e_y,\n(select zpos from  v_element where oid=C.to_e) as to_e_z,\nget_cab_length (C.SEQID,C.FROM_E,C.TO_E,C.F_ROUT,C.PERC_CORR,C.EXT_LEN_1,C.EXT_LEN_2,C.EST_LENG) as len,\nget_cab_route_area(C.seqid) as cablepath,\n(select ADDITIONAL_DATA from  CAB_SPEC where SEQID=C.spec) as adddata,\n(select PARTNUMBER from  V_SECTION_SPEC where spec_oid=C.spec and sect_oid=C.sect) as partnumber,\nss.CODENUMBER  as stockcode\nfrom cable C, SECTION_SPEC SS\nwhere \n(C.spec=SS.spec and SS.nom_sect=C.sect)\n)\n)\nwhere \n(FROM_N_ROOM='${room}' OR FROM_E_ROOM='${room}' OR TO_N_ROOM='${room}' OR TO_E_ROOM='${room}')\norder by code\n\n "

  def retrieveCablesByComplect(project: String, complectName: String): List[Cable] = {
    val regexp: Regex = "\\s\\d{6}\\s".r
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buff = ListBuffer.empty[Cable]
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = retrieveCablesByComplectSql(complectName)
          val rs: ResultSet = stmt.executeQuery(sql)
          val wmats = retrieveAllMaterialsByProject(project)
          while (rs.next()) {
            val path: String = Option(rs.getString("CABLEPATH")).getOrElse("")
            regexp.findFirstMatchIn(path) match {
              case Some(_) => None
              case None => {
                val stockCode = Option(rs.getString("STOCKCODE")).getOrElse("")
                val wmat: WorkShopMaterial = findWorkshopMaterialEquals(stockCode, wmats)
                val cable = Cable(
                  Option(rs.getInt("SEQID")).getOrElse(0),
                  Option(rs.getString("CODE")).getOrElse(""),
                  Option(rs.getInt("SPEC")).getOrElse(0),
                  Option(rs.getInt("SECT")).getOrElse(0),
                  Option(rs.getString("SECTION")).getOrElse(""),
                  Option(rs.getString("MARK")).getOrElse(""),
                  Option(rs.getString("SCHEME")).getOrElse(""),
                  Option(rs.getString("FROM_N")).getOrElse(""),
                  Option(rs.getString("FROM_N_ROOM")).getOrElse(""),
                  Option(rs.getDouble("FROM_N_X")).getOrElse(0.0),
                  Option(rs.getDouble("FROM_N_Y")).getOrElse(0.0),
                  Option(rs.getDouble("FROM_N_Z")).getOrElse(0.0),
                  Option(rs.getString("FROM_E_USERID")).getOrElse(""),
                  Option(rs.getString("FROM_E_USERID_ELEC")).getOrElse(""),
                  Option(rs.getString("FROM_E_ROOM")).getOrElse(""),
                  Option(rs.getDouble("FROM_E_X")).getOrElse(0.0),
                  Option(rs.getDouble("FROM_E_Y")).getOrElse(0.0),
                  Option(rs.getDouble("FROM_E_Z")).getOrElse(0.0),
                  Option(rs.getString("TO_E_USERID")).getOrElse(""),
                  Option(rs.getString("TO_N")).getOrElse(""),
                  Option(rs.getString("TO_N_ROOM")).getOrElse(""),
                  Option(rs.getDouble("TO_N_X")).getOrElse(0.0),
                  Option(rs.getDouble("TO_N_Y")).getOrElse(0.0),
                  Option(rs.getDouble("TO_N_Z")).getOrElse(0.0),
                  Option(rs.getString("TO_E_USERID_ELEC")).getOrElse(""),
                  Option(rs.getString("TO_E_ROOM")).getOrElse(""),
                  Option(rs.getDouble("TO_E_X")).getOrElse(0.0),
                  Option(rs.getDouble("TO_E_Y")).getOrElse(0.0),
                  Option(rs.getDouble("TO_E_Z")).getOrElse(0.0),
                  Option(rs.getDouble("LEN")).getOrElse(0.0),
                  path,
                  Option(rs.getString("ADDDATA")).getOrElse(""),
                  Option(rs.getString("PARTNUMBER")).getOrElse(""),
                  stockCode,
                  complectName,
                  wmat
                )
                buff += cable
              }
            }
          }
          rs.close()
          stmt.close()
          connection.close()
          buff.toList
        }
        catch {
          case e: Exception =>
            connection.close()
            List.empty[Cable]
        }
      }
      case None => List.empty[Cable]
    }
  }

  def retrieveCablesByComplectMagistralVariant(project: String, complectName: String): List[Cable] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buff = ListBuffer.empty[Cable]
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = retrieveCablesByComplectMagistralVariantSQL(complectName)
          val rs: ResultSet = stmt.executeQuery(sql)
          val wmats = retrieveAllMaterialsByProject(project)
          while (rs.next()) {
            val path: String = Option(rs.getString("CABLEPATH")).getOrElse("")
            val stockCode = Option(rs.getString("STOCKCODE")).getOrElse("")
            val wmat: WorkShopMaterial = findWorkshopMaterialEquals(stockCode, wmats)
            val cable = Cable(
              Option(rs.getInt("SEQID")).getOrElse(0),
              Option(rs.getString("CODE")).getOrElse(""),
              Option(rs.getInt("SPEC")).getOrElse(0),
              Option(rs.getInt("SECT")).getOrElse(0),
              Option(rs.getString("SECTION")).getOrElse(""),
              Option(rs.getString("MARK")).getOrElse(""),
              Option(rs.getString("SCHEME")).getOrElse(""),
              Option(rs.getString("FROM_N")).getOrElse(""),
              Option(rs.getString("FROM_N_ROOM")).getOrElse(""),
              Option(rs.getDouble("FROM_N_X")).getOrElse(0.0),
              Option(rs.getDouble("FROM_N_Y")).getOrElse(0.0),
              Option(rs.getDouble("FROM_N_Z")).getOrElse(0.0),
              Option(rs.getString("FROM_E_USERID")).getOrElse(""),
              Option(rs.getString("FROM_E_USERID_ELEC")).getOrElse(""),
              Option(rs.getString("FROM_E_ROOM")).getOrElse(""),
              Option(rs.getDouble("FROM_E_X")).getOrElse(0.0),
              Option(rs.getDouble("FROM_E_Y")).getOrElse(0.0),
              Option(rs.getDouble("FROM_E_Z")).getOrElse(0.0),
              Option(rs.getString("TO_E_USERID")).getOrElse(""),
              Option(rs.getString("TO_N")).getOrElse(""),
              Option(rs.getString("TO_N_ROOM")).getOrElse(""),
              Option(rs.getDouble("TO_N_X")).getOrElse(0.0),
              Option(rs.getDouble("TO_N_Y")).getOrElse(0.0),
              Option(rs.getDouble("TO_N_Z")).getOrElse(0.0),
              Option(rs.getString("TO_E_USERID_ELEC")).getOrElse(""),
              Option(rs.getString("TO_E_ROOM")).getOrElse(""),
              Option(rs.getDouble("TO_E_X")).getOrElse(0.0),
              Option(rs.getDouble("TO_E_Y")).getOrElse(0.0),
              Option(rs.getDouble("TO_E_Z")).getOrElse(0.0),
              Option(rs.getDouble("LEN")).getOrElse(0.0),
              path,
              Option(rs.getString("ADDDATA")).getOrElse(""),
              Option(rs.getString("PARTNUMBER")).getOrElse(""),
              stockCode,
              complectName,
              wmat
            )
            buff += cable
          }
          rs.close()
          stmt.close()
          connection.close()
          buff.toList
        }
        catch {
          case e: Exception =>
            connection.close()
            List.empty[Cable]
        }
      }
      case None => List.empty[Cable]
    }
  }

  def retrieveCablesByRoom(project: String, room: String): List[Cable] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buff = ListBuffer.empty[Cable]
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = retrieveCablesByRoomSql(room)
          val rs: ResultSet = stmt.executeQuery(sql)
          val wmats = retrieveAllMaterialsByProject(project)
          while (rs.next()) {
            val path: String = Option(rs.getString("CABLEPATH")).getOrElse("")
            val stockCode = Option(rs.getString("STOCKCODE")).getOrElse("")
            val wmat: WorkShopMaterial = findWorkshopMaterialEquals(stockCode, wmats)
            val cable = Cable(
              Option(rs.getInt("SEQID")).getOrElse(0),
              Option(rs.getString("CODE")).getOrElse(""),
              Option(rs.getInt("SPEC")).getOrElse(0),
              Option(rs.getInt("SECT")).getOrElse(0),
              Option(rs.getString("SECTION")).getOrElse(""),
              Option(rs.getString("MARK")).getOrElse(""),
              Option(rs.getString("SCHEME")).getOrElse(""),
              Option(rs.getString("FROM_N")).getOrElse(""),
              Option(rs.getString("FROM_N_ROOM")).getOrElse(""),
              Option(rs.getDouble("FROM_N_X")).getOrElse(0.0),
              Option(rs.getDouble("FROM_N_Y")).getOrElse(0.0),
              Option(rs.getDouble("FROM_N_Z")).getOrElse(0.0),
              Option(rs.getString("FROM_E_USERID")).getOrElse(""),
              Option(rs.getString("FROM_E_USERID_ELEC")).getOrElse(""),
              Option(rs.getString("FROM_E_ROOM")).getOrElse(""),
              Option(rs.getDouble("FROM_E_X")).getOrElse(0.0),
              Option(rs.getDouble("FROM_E_Y")).getOrElse(0.0),
              Option(rs.getDouble("FROM_E_Z")).getOrElse(0.0),
              Option(rs.getString("TO_E_USERID")).getOrElse(""),
              Option(rs.getString("TO_N")).getOrElse(""),
              Option(rs.getString("TO_N_ROOM")).getOrElse(""),
              Option(rs.getDouble("TO_N_X")).getOrElse(0.0),
              Option(rs.getDouble("TO_N_Y")).getOrElse(0.0),
              Option(rs.getDouble("TO_N_Z")).getOrElse(0.0),
              Option(rs.getString("TO_E_USERID_ELEC")).getOrElse(""),
              Option(rs.getString("TO_E_ROOM")).getOrElse(""),
              Option(rs.getDouble("TO_E_X")).getOrElse(0.0),
              Option(rs.getDouble("TO_E_Y")).getOrElse(0.0),
              Option(rs.getDouble("TO_E_Z")).getOrElse(0.0),
              Option(rs.getDouble("LEN")).getOrElse(0.0),
              path,
              Option(rs.getString("ADDDATA")).getOrElse(""),
              Option(rs.getString("PARTNUMBER")).getOrElse(""),
              stockCode,
              "",
              wmat
            )
            buff += cable
          }
          rs.close()
          stmt.close()
          connection.close()
          buff.toList
        }
        catch {
          case e: Exception =>
            connection.close()
            List.empty[Cable]
        }
      }
      case None => List.empty[Cable]
    }
  }


}
