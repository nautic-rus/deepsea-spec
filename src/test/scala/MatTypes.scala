import deepsea.database.DBManager
import deepsea.esp.EspManagerHelper
import deepsea.pipe.PipeHelper
import deepsea.pipe.PipeManager.{PipeSeg, Pls, PlsElem, PlsParam}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class MatTypes extends AnyFunSuite with PipeHelper with EspManagerHelper{
  val project = "P701"

  val sections = List(ProfileSection(0, "FS"),
                      ProfileSection(1, "AS"),
                      ProfileSection(2, "IS"),
                      ProfileSection(3, "TS"),
                      ProfileSection(4, "US"),
                      ProfileSection(5, "BS"),
                      ProfileSection(6, "ST"),
                      ProfileSection(7, "AT"),
                      ProfileSection(8, "OS"),
                      ProfileSection(9, "PS"),
                      ProfileSection(10, "RS"),
                      ProfileSection(11, "MC"),
                      ProfileSection(12, "DB"),
                      ProfileSection(13, "SR"),
                      ProfileSection(14, "HR"),
                      ProfileSection(15, "LI"),
                      ProfileSection(16, "ZL"),
                      ProfileSection(17, "TL"))
  case class ProfileSection(code: Int, section: String)
  case class HullSpec(descr: String, partType: String, thickness: Double, kse: Double, hullKse: HullKse)
  case class PipeSpec(t: String, tDesc: String, nd: Double, outD: Double, thick: Double)
  case class HvacSpec(name: String, cType: String, oid: Double, params: List[HvacParam])
  case class HvacPlates(name: String, oid: Int)
  case class HvacParam(oid: Double, name: String, pType: String, value: String)
  case class HullKse(kse: Double, section: Int, wH: Double, wT: Double, fH: Double, fT: Double)
  case class NameDim(name: String, dim: String)
  case class CabSpec(code: String, spec: String, addData: String, oDiam: Double, nomSect: Double)
  case class TraySpec(descr: String, w: Double, h: Double, diam: Double, fillRule: String, nomSize: String, cabWidth: Double, cabHeight: Double, trayComp: String)
  case class PipeJoin(nomDiam: Double, joinSpec: String, joinPressure: String, joinThick: Double, nuts: Double, nutsPressure: String, boltLength: Double, boltPressure: String, bolts: Double)

  //printHullWithCount(getHull(project))
  //printHull(getHullByBlock(project, "U0107"))
  //printPipe(getPipe(project))
  //printHvac(getHvac(project))
  //printCables(getCables(project))
  //printCableTrays(getCableTrays(project))
  //printPipeJoins(getPipeJoins(project))
  printHvacPlates(getHvacPlates(project))
  //printPipeWithCount(getPipeBySystem(project, "813-1001"))

  //val materials = getMaterialsAux.filter(_.project == "200101")
//  val esps = getGlobalEsp(1)
//  esps.sortBy(_.name).foreach(m => {
//    println(List(m.code, m.name.replace(",", ""), m.weight, m.units, m.qty, m.weightTotal).mkString(","))
//  })
  //val espMaterials = getGlobalEsp(1).map(_.code)

//  materials.filter(x => espMaterials.contains(x.code)).sortBy(_.name).foreach(m => {
//    println(List(m.code, m.name("ru").replace(",", "").trim, m.units, m.singleWeight).mkString(","))
//  })

  val q = 0


  def getHull(project: String): List[HullSpec] = {
    val kses = getKse(project)
    val res = ListBuffer.empty[HullSpec]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val rs = stmt.executeQuery("select * from v_inp_single_part")
        while (rs.next()){
          val kse: Double = Option(rs.getDouble("KSE")).getOrElse(0)
          res += HullSpec(
            rs.getString("DESCRIPTION"),
            rs.getString("PART_TYPE"),
            rs.getDouble("THICKNESS"),
            kse,
            kses.find(_.kse == kse) match {
              case Some(value) => value
              case _ => HullKse(0, 0, 0, 0, 0, 0)
            }
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }
  def getHullByBlock(project: String, block: String): List[HullSpec] = {
    val kses = getKse(project)
    val res = ListBuffer.empty[HullSpec]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val rs = stmt.executeQuery("select * from v_inp_single_part where block = '" + block + "'")
        while (rs.next()){
          val kse: Double = Option(rs.getDouble("KSE")).getOrElse(0)
          res += HullSpec(
            rs.getString("DESCRIPTION"),
            rs.getString("PART_TYPE"),
            rs.getDouble("THICKNESS"),
            kse,
            kses.find(_.kse == kse) match {
              case Some(value) => value
              case _ => HullKse(0, 0, 0, 0, 0, 0)
            }
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }
  def getKse(project: String): List[HullKse] = {
    val res = ListBuffer.empty[HullKse]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val rs = stmt.executeQuery("select * from STD_PROFILE")
        while (rs.next()){
          res += HullKse(
            Option(rs.getDouble("KSE")).getOrElse(0),
            Option(rs.getInt("SECTION")).getOrElse(0),
            Option(rs.getDouble("WEB_HEIGHT")).getOrElse(0),
            Option(rs.getDouble("WEB_THICKNESS")).getOrElse(0),
            Option(rs.getDouble("FLANGE_HEIGHT")).getOrElse(0),
            Option(rs.getDouble("FLANGE_THICKNESS")).getOrElse(0)
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }
  def printHull(hull: List[HullSpec]): Unit = {
    hull.groupBy(x => (x.descr, x.thickness, x.kse)).map(group => {
      val thick = if (group._1._3 != 0){
        val k = group._2.head.hullKse
        val s = sections.find(_.code == k.section) match {
          case Some(value) => value.section
          case _ => ""
        }
        (List(s) ++ List(k.wH, k.wT, k.fH, k.fT).map(_.toInt).filter(_ > 0)).mkString("x")
      }
      else{
        "s" + group._1._2.toInt.toString
      }
      List(group._1._1, thick).mkString(",")
    }).toList.sortBy(x => x).foreach(println)
  }
  def printHullWithCount(hull: List[HullSpec]): Unit = {
    hull.groupBy(x => (x.descr, x.thickness, x.kse)).map(group => {
      val thick = if (group._1._3 != 0){
        val k = group._2.head.hullKse
        val s = sections.find(_.code == k.section) match {
          case Some(value) => value.section
          case _ => ""
        }
        (List(s) ++ List(k.wH, k.wT, k.fH, k.fT).map(_.toInt).filter(_ > 0)).mkString("x")
      }
      else{
        "s" + group._1._2.toInt.toString
      }
      List(group._1._1, thick, group._2.length).mkString(",")
    }).toList.sortBy(x => x).foreach(println)
  }
  def getPipe(project: String): List[PipeSpec] = {
    val res = ListBuffer.empty[PipeSpec]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val rs = stmt.executeQuery("select * from V_PIPECOMP")
        while (rs.next()){
          res += PipeSpec(
            rs.getString("TYPECODE"),
            rs.getString("TYPEDESC"),
            rs.getDouble("ND1MM"),
            rs.getDouble("OUTDIAMETER"),
            rs.getDouble("THICKNESS"),
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }
  def printPipe(pipe: List[PipeSpec]): Unit = {
    pipe.groupBy(x => (x.t, x.tDesc, x.nd, x.outD, x.thick)).toList.sortBy(x => x._1._2 + "-" + x._1._3 + "-" + x._1._4 + "-" + x._1._5).foreach(g => {
      println(List(g._1._2, g._1._3, g._1._4, g._1._5).mkString(","))
    })
  }
  def getPipeBySystem(project: String, system: String): List[PipeSpec] = {
    val res = ListBuffer.empty[PipeSpec]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val rs = stmt.executeQuery("select * from V_PIPECOMP where systemname = '" + system + "'")
        while (rs.next()){
          res += PipeSpec(
            rs.getString("TYPECODE"),
            rs.getString("TYPEDESC"),
            rs.getDouble("ND1MM"),
            rs.getDouble("OUTDIAMETER"),
            rs.getDouble("THICKNESS"),
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }
  def printPipeWithCount(pipe: List[PipeSpec]): Unit = {
    pipe.groupBy(x => (x.t, x.tDesc, x.nd, x.outD, x.thick)).toList.sortBy(x => x._1._2 + "-" + x._1._3 + "-" + x._1._4 + "-" + x._1._5).foreach(g => {
      println(List(g._1._2, g._1._3, g._1._4, g._1._5, g._2.length).mkString(","))
    })
  }
  def getHvac(project: String): List[HvacSpec] = {
    val res = ListBuffer.empty[HvacSpec]
    val params = getHvacParams(project)
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val rs = stmt.executeQuery("select * from HVACCMP_COMPONENT")
        while (rs.next()){
          val id = rs.getDouble("OID")
          res += HvacSpec(
            rs.getString("NAME").replace(",", " "),
            rs.getString("CMP_TYPE") match {
              case "F" => "Flange"
              case "D" => "Pipe"
              case "E" => "Elbow"
              case _ => "Unknown"
            },
            id,
            params.filter(_.oid == id)
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }
  def getHvacParams(project: String): List[HvacParam] = {
    val res = ListBuffer.empty[HvacParam]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val rs = stmt.executeQuery("select * from HVACCMP_COMPONENT_PARAM")
        while (rs.next()){
          res += HvacParam(
            rs.getDouble("CMP_OID"),
            Option(rs.getString("NAME")).getOrElse(""),
            Option(rs.getString("TYPE")).getOrElse(""),
            Option(rs.getString("VALUE")).getOrElse(""),
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }
  def printHvac(hvac: List[HvacSpec]): Unit = {
    hvac.sortBy(_.name).foreach(x => {
      val n = x.name
      val cType = x.cType
      val params = x.params.map(p => {
        (if (p.pType != "") p.pType else "X:") +
        (if (p.name != "") "(" + p.name + ")" else "") +
        ":" + p.value
      }).mkString("; ")
      println(List(n, cType, params).mkString(","))
    })
  }
  def getCables(project: String): List[CabSpec] = {
    val res = ListBuffer.empty[CabSpec]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val q = "select * from cable cab\nleft join cab_spec cs on cs.SEQID = cab.SPEC\nleft join section_spec ss on ss.spec = cab.SPEC and ss.nom_sect = cab.sect\nleft join nom_section ns on ns.seqid = ss.NOM_SECT"
        val rs = stmt.executeQuery(q)
        while (rs.next()){
          res += CabSpec(
            Option(rs.getString("CODE")).getOrElse(""),
            Option(rs.getString("DESCR")).getOrElse("").replace(",", " "),
            Option(rs.getString("ADDITIONAL_DATA")).getOrElse("").replace(",", " "),
            Option(rs.getDouble("O_DIAMETER")).getOrElse(0),
            Option(rs.getDouble("DIAM")).getOrElse(0),
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }
  def printCables(cab: List[CabSpec]): Unit = {
    cab.groupBy(x => (x.spec, x.addData, x.oDiam, x.nomSect)).toList.sortBy(x => x._1._1 + "-" + x._1._2 + "-" + x._1._3 + "-" + x._1._4).foreach(g => {
      println(List(g._1._1, g._1._2, g._1._3, g._1._4).mkString(","))
    })
  }
  def getCableTrays(project: String): List[TraySpec] = {
    val res = ListBuffer.empty[TraySpec]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val q = "select * from V_CTRAY_PATTERN_LEVEL"
        val rs = stmt.executeQuery(q)
        while (rs.next()){
          res += TraySpec(
            Option(rs.getString("DESCR")).getOrElse("").replace(",", " "),
            Option(rs.getDouble("WIDTH")).getOrElse(0),
            Option(rs.getDouble("HEIGHT")).getOrElse(0),
            Option(rs.getDouble("DIAMETER")).getOrElse(0),
            Option(rs.getString("FILL_RULE_DESCR")).getOrElse("").replace(",", " "),
            Option(rs.getString("NOM_SIZE_DESCR")).getOrElse("").replace(",", " "),
            Option(rs.getDouble("CABLE_WIDTH")).getOrElse(0),
            Option(rs.getDouble("CABLE_HEIGHT")).getOrElse(0),
            Option(rs.getString("TRAY_COMPONENT")).getOrElse("").replace(",", " "),
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }
  def printCableTrays(cab: List[TraySpec]): Unit = {
    cab.sortBy(x => x.descr + "-" + x.nomSize).foreach(g => {
      println(List(g.descr, g.nomSize, g.trayComp, g.fillRule, g.w, g.h, g.cabWidth, g.cabHeight).mkString(","))
    })
  }
  def getHvacPlates(project: String): List[String] = {
    val res = ListBuffer.empty[String]
    val plsElems = ListBuffer.empty[PlsElem]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val q = "SELECT\n    PLS.*,\n    SYS.*,\n    CMP.*,\n    ELE.*,\n    ZON.USERID AS ZONEUSERID,\n    SPL.USERID AS SPOOL,\n    ISO.USERID AS ISOM,\n    CMP.NAME AS CMPNAME\nFROM\n    PLS_ELEM PLS\n        LEFT JOIN SYSTEMS SYS ON SYS.SEQID = PLS.SYSTEM\n        LEFT JOIN ZONE ZON ON ZON.SEQID = PLS.ZONE\n        LEFT JOIN SPOOL SPL ON SPL.ID = PLS.SPOOLID AND SPL.ZONE = PLS.ZONE AND SPL.SYSTEM = PLS.SYSTEM\n        LEFT JOIN ISOMETRIC ISO ON ISO.ID = PLS.ISOMID AND ISO.ZONE = PLS.ZONE\n        LEFT JOIN HVACCMP_COMPONENT CMP ON CMP.OID = PLS.CMP_OID\n        LEFT JOIN ELEMENT_LANG ELE ON ELE.ELEM = PLS.OID\nWHERE PLS.SPOOLID IS NOT NULL"
        val rs = stmt.executeQuery(q)
        while (rs.next()) {
          plsElems += PlsElem(Pls(Option(rs.getInt("TYPE")).getOrElse(0),
            Option(rs.getInt("ZONE")).getOrElse(0),
            Option(rs.getInt("SYSTEM")).getOrElse(0),
            Option(rs.getString("LINE")).getOrElse(""),
            Option(rs.getInt("PLS")).getOrElse(0),
            Option(rs.getInt("ELEM")).getOrElse(0)),
            Option(rs.getDouble("WEIGHT")).getOrElse(0),
            Option(rs.getInt("ISOMID")).getOrElse(0),
            Option(rs.getInt("SPOOLID")).getOrElse(0),
            Option(rs.getInt("ISPIECEID")).getOrElse(0),
            Option(rs.getInt("SPPIECEID")).getOrElse(0),
            Option(rs.getString("CTYPE")).getOrElse(""),
            Option(rs.getInt("IDSQ")).getOrElse(0),
            Option(rs.getInt("CMP_OID")).getOrElse(0),
            Option(rs.getString("STOCK_CODE")).getOrElse(""),
            Option(rs.getString("ZONEUSERID")).getOrElse(""),
            Option(rs.getString("SPOOL")).getOrElse(""),
            Option(rs.getString("ISOM")).getOrElse(""),
            Option(rs.getString("DESCR")).getOrElse(""),
            Option(rs.getString("CMPNAME")).getOrElse(""))
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    val plsParams = ListBuffer.empty[PlsParam]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val q = "SELECT * FROM PLSE_PAROBJ_REALPAR"
        val rs = stmt.executeQuery(q)
        while (rs.next()) {
          plsParams += PlsParam(
            Pls(Option(rs.getInt("TYPE")).getOrElse(0),
              Option(rs.getInt("ZONE")).getOrElse(0),
              Option(rs.getInt("SYSTEM")).getOrElse(0),
              Option(rs.getString("LINE")).getOrElse(""),
              Option(rs.getInt("PLS")).getOrElse(0),
              Option(rs.getInt("ELEM")).getOrElse(0)),
            Option(rs.getInt("PARAM_OBJ")).getOrElse(0),
            Option(rs.getInt("PARAM_SQ")).getOrElse(0),
            Option(rs.getDouble("VALUE")).getOrElse(0),
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    plsElems.filter(_.cmp_oid == 0).foreach(plsElem => {
      val params = plsParams.filter(_.pls.equals(plsElem.pls)).sortBy(_.paramSq)
      val hvacName = plsElem.cType match {
        case "B" =>
          if (params.length == 3){
            val radius = Math.round(params(0).value * 2)
            val diam = Math.round(params(1).value)
            val angle = Math.round(180 / Math.PI * params(2).value)
            s"ОТВОД $angle° ДУ$radius"
          }
          else if (params.length == 4) {
            val d1 = Math.round(params(0).value)
            val d2 = Math.round(params(1).value)
            val angle = Math.round(180 / Math.PI * params(3).value)
            s"ОТВОД $angle° ${d1}x$d2"
          }
          else{
            "undefined"
          }
        case "A" =>
          if (params.length == 2) {
            val d1 = Math.round(params(0).value * 2)
            val d2 = Math.round(params(1).value * 2)
            s"ПЕРЕХОД ДУ$d2/ДУ$d1"
          }
          else if (params.length == 3){
            val d1 = Math.round(params(0).value * 2)
            val d2 = Math.round(params(1).value * 2)
            val l = Math.round(params(2).value)
            s"ПЕРЕХОД ДУ$d2/ДУ$d1"
          }
          else if (params.length == 7) {
            val d1 = Math.round(params(0).value)
            val d2 = Math.round(params(1).value)
            val d3 = Math.round(params(2).value)
            val d4 = Math.round(params(3).value)
            val l = Math.round(params.last.value)
            s"ПЕРЕХОД ${d1}x$d2/${d3}x$d4"
          }
          else if (params.length == 8) {
            val d1 = Math.round(params(0).value)
            val d2 = Math.round(params(1).value)
            val diam = Math.round(params(2).value)
            val l = Math.round(params(3).value)
            s"ПЕРЕХОД ${d1}x$d2/ДУ$diam"
          }
          else if (params.length == 9) {
            val d1 = Math.round(params(0).value)
            val d2 = Math.round(params(1).value)
            val r = Math.round(params(2).value)
            val angle = Math.round(180 / Math.PI * params(3).value)
            val d3 = Math.round(params(4).value)
            val d4 = Math.round(params(5).value)
            s"ОТВОД $angle° R=$r С ПЕРЕХОДОМ ${d1}x$d2/${d3}x$d4"
          }
          else if (params.length > 10) {
            val d1 = Math.round(params(0).value)
            val d2 = Math.round(params(1).value)
            val diam = Math.round(params(2).value * 2)
            val l = Math.round(params(3).value)
            s"ПЕРЕХОД ${d2}x$d1/ДУ$diam"
          }
          else{
            "undefined"
          }
        case "P" =>
          if (params.length == 2){
            val d = Math.round(params(0).value * 2)
            val l = Math.round(params(1).value)
            s"ВОЗДУХОВОД ДУ$d" + "&l=" + l
          }
          else if (params.length == 3){
            val d1 = Math.round(params(0).value)
            val d2 = Math.round(params(1).value)
            val l = Math.round(params(2).value)
            s"ВОЗДУХОВОД ${d2}х${d1}" + "&l=" + l
          }
          else{
            "undefined"
          }
        case _ => ""
      }
      res += hvacName
    })
    res.toList.filter(_ != "undefined").distinct
  }
  def printHvacPlates(hvac: List[String]): Unit = {
    val print = ListBuffer.empty[Tuple2[String, Double]]
    hvac.foreach(h => {
      if (h.contains("&l=")){
        val s: Array[String] = h.split("&")
        val l: Double = s.last.replace("l=", "").toDoubleOption.getOrElse(0)
        print += Tuple2(s.head, l)
      }
      else{
        print += Tuple2(h, 0)
      }
    })
    print.groupBy(_._1).toList.sortBy(_._1).map(gr => {
      List(gr._1, gr._2.map(_._2).sum).mkString(",")
    }).foreach(println)
  }
  def getPipeJoins(project: String): List[PipeJoin] = {
    val res = ListBuffer.empty[PipeJoin]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val q = "select * from V_PIPEJOINS"
        val rs = stmt.executeQuery(q)
        while (rs.next()){
          res += PipeJoin(
            Option(rs.getDouble("NDMM")).getOrElse(0),
            Option(rs.getString("JOINSPEC")).getOrElse(""),
            Option(rs.getString("JOINPRESSURE")).getOrElse(""),
            Option(rs.getDouble("JOINTHICKNESS")).getOrElse(0),
            Option(rs.getDouble("NUTS1NUMBER")).getOrElse(0),
            Option(rs.getString("NUTS1PRESSURE")).getOrElse(""),
            Option(rs.getDouble("BOLTS1LENGTH")).getOrElse(0),
            Option(rs.getString("BOLTS1PRESSURE")).getOrElse(""),
            Option(rs.getDouble("BOLTS1NUMBER")).getOrElse(0),
          )
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }
  def printPipeJoins(cab: List[PipeJoin]): Unit = {
    cab.groupBy(x => (x.nomDiam, x.joinSpec, x.joinPressure, x.joinThick, x.nuts, x.nutsPressure, x.boltLength, x.boltPressure, x.bolts)).foreach(g => {
      println(List(g._1._1, g._1._2, g._1._3, g._1._4, g._1._5, g._1._6, g._1._7, g._1._8, g._1._9).mkString(","))
    })
  }
  def getProjectSystems(project: String): List[String] = {
    val res = ListBuffer.empty[String]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val q = "select * from systems"
        val rs = stmt.executeQuery(q)
        while (rs.next()) {
          res += Option(rs.getString("NAME")).getOrElse("")
        }
        stmt.close()
        conn.close()
      case _ => None
    }
    res.toList
  }

}

