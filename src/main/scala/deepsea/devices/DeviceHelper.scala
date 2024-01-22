package deepsea.devices

import deepsea.accomodations.AccommodationHelper
import deepsea.database.DBManager
import deepsea.devices.DeviceManager.{Device, DeviceAux, SystemLang}
import deepsea.pipe.PipeManager.{Material, PipeSeg, ProjectName, SystemDef, Units}
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.{and, equal, notEqual}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait DeviceHelper extends AccommodationHelper {

  def getDevices(docNumber: String): List[Device] ={
    val devices = ListBuffer.empty[Device]
    val devicesAux = ListBuffer.empty[DeviceAux]
    val devicesAuxFromSystem = ListBuffer.empty[DeviceAux]
    val devicesAuxFromComp = ListBuffer.empty[Device]
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val materialsNCollectionName = "materials-n"
        val materialsCollection: MongoCollection[Material] = mongo.getCollection(materialsNCollectionName)
        val projectNamesCollection: MongoCollection[ProjectName] = mongo.getCollection("project-names")
        val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[ProjectName] => values.toList
          case _ => List.empty[ProjectName]
        }
        val rkdProject = if (docNumber.contains('-')) docNumber.split('-').head else ""
        val foranProject = if ("""200101-100-10[0-9]""".r.matches(docNumber)) "NT02" else projectNames.find(_.rkd == rkdProject) match {
          case Some(value) => value.foran
          case _ => ""
        }
        val materials = Await.result(materialsCollection.find(equal("project", rkdProject)).toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[Material] => values.toList
          case _ => List.empty[Material]
        }
        val systemDefs = getDeviceSystemDefs(foranProject)
        val system = systemDefs.find(_.descr.contains(docNumber)) match {
          case Some(value) =>
            value.name
          case _ => ""
        }
        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracle) =>
            val s = oracle.createStatement()
            //val query = s"select * from v_element_desc where syst_userid = '$system'"
            //val query = s"select \n    oid, \n    comp, \n    userid, \n    syst_userid, \n    zone_userid, \n    type, \n    comp_abbrev, \n    weight, \n    stock_code, \n    elem_class,\n    elem_desc1, \n    elem_desc2, \n    stock_code, \n    (select long_descr from COMPONENT_LANG cl where lang = -2 and cl.comp = elemdesc.comp and rownum = 1) as long_desc\nfrom \n    v_element_desc  elemdesc\nwhere \n    syst_userid = '$system'"
            val query = s"select \n    oid, \n    comp, \n    userid, \n    syst_userid, \n    zone_userid, \n    type, \n    comp_abbrev, \n    weight, \n    stock_code, \n    elem_class,\n    elem_desc1, \n    elem_desc2, \n    stock_code, \n    (select long_descr from COMPONENT_LANG cl where lang = -2 and cl.comp = elemdesc.comp and rownum = 1) as long_desc,\n    (select long_descr from element_lang cl where lang = -2 and cl.elem = elemdesc.oid and rownum = 1) as long_desc_elem\nfrom \n    v_element_desc  elemdesc\nwhere \n    syst_userid = '$system'"
            val rs = s.executeQuery(query)
            while (rs.next()) {
              devices += Device(
                foranProject,
                Option(rs.getInt("OID")).getOrElse(-1),
                Option(rs.getInt("COMP")).getOrElse(-1),
                Option(rs.getString("USERID")).getOrElse(""),
                Option(rs.getString("SYST_USERID")).getOrElse(""),
                Option(rs.getString("ZONE_USERID")).getOrElse(""),
                Option(rs.getString("TYPE")).getOrElse(""),
                Option(rs.getString("COMP_ABBREV")).getOrElse(""),
                Option(rs.getDouble("WEIGHT")).getOrElse(-1),
                Option(rs.getString("STOCK_CODE")).getOrElse(""),
                Option(rs.getInt("ELEM_CLASS")).getOrElse(-1),
                Option(rs.getString("ELEM_DESC1")).getOrElse(""),
//                Option(rs.getString("ELEM_DESC2")).getOrElse(""),
                Option(rs.getString("LONG_DESC_ELEM")).getOrElse(""),
                Option(rs.getString("LONG_DESC")).getOrElse(""),
//                Option(rs.getString("LONG_DESC_ELEM")).getOrElse(""),
                materials.find(_.code == Option(rs.getString("STOCK_CODE")).getOrElse("")) match {
                  case Some(value) => value
                  case _ => Material()
                },
                Option(rs.getString("USERID")).getOrElse(""),
                "")
            }
            rs.close()
            s.close()
            oracle.close()
          case _ => List.empty[Device]
        }
        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracle) =>
            val s = oracle.createStatement()
            val query = s"select elem, long_descr from element_lang where elem in (select oid from v_element_desc where syst_userid = '$system') and lang = -1 and long_descr is not null"
            val rs = s.executeQuery(query)
            while (rs.next()) {
              devicesAux += DeviceAux(
                Option(rs.getInt("ELEM")).getOrElse(-1),
                Option(rs.getString("LONG_DESCR")).getOrElse("").replace("\r", ""),
              )
            }
            rs.close()
            s.close()
            oracle.close()
          case _ => List.empty[Device]
        }
        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracle) =>
            val s = oracle.createStatement()
            val query = s"select system, long_descr from systems_lang where system in (select oid from systems where name = '$system')"
            val rs = s.executeQuery(query)
            while (rs.next()) {
              devicesAuxFromSystem += DeviceAux(
                Option(rs.getInt("SYSTEM")).getOrElse(-1),
                Option(rs.getString("LONG_DESCR")).getOrElse("").replace("\r", ""),
              )
            }
            rs.close()
            s.close()
            oracle.close()
          case _ => List.empty[Device]
        }
        devicesAux.filter(_.descr.contains("|")).foreach(d => {
          d.descr.split('\n').toList.foreach(l => {
            val split = l.split('|')
            devices.find(x => x.id == d.id && x.fromAux == 0) match {
              case Some(deviceBase) =>
                if (split.length == 4){
                  devices += Device(
                    deviceBase.project,
                    d.id,
                    deviceBase.comp,
                    split(0),
                    deviceBase.system,
                    deviceBase.zone,
                    deviceBase.elemType,
                    deviceBase.compAbbrev,
                    split(2) match {
                      case "166" => split(3).toDoubleOption.getOrElse(0)
                      case _ => materials.find(_.code == split(1)) match {
                        case Some(value) => value.singleWeight
                        case _ => 0
                      }
                    },
                    split(1),
                    deviceBase.elemClass,
                    "",
                    "",
                    "",
//                    "",
                    materials.find(_.code == split(1)) match {
                      case Some(value) => value
                      case _ => Material()
                    },
                    split(0),
                    deviceBase.userId,
                    split(2),
                    split(3).toDoubleOption.getOrElse(0),
                    1,
                  )
                }
              case _ => None
            }
          })
        })
        devicesAuxFromSystem.filter(_.descr.contains("|")).foreach(d => {
          d.descr.split('\n').toList.foreach(l => {
            val split = l.split('|')
            if (split.length == 4){
              devices += Device(
                rkdProject,
                d.id,
                0,
                split(0),
                system,
                devices.find(x => split(0).contains(x.userId)) match {
                  case Some(value) => value.zone
                  case _ => ""
                },
                "",
                "",
                materials.find(_.code == split(1)) match {
                  case Some(value) => value.singleWeight
                  case _ => 0
                },
                split(1),
                0,
                "",
                "",
                "",
//                "",
                materials.find(_.code == split(1)) match {
                  case Some(value) => value
                  case _ => Material()
                },
                split(0),
                "",
                split(2),
                split(3).toDoubleOption.getOrElse(0),
                1
              )
            }
          })
        })
        devices.foreach(d => {
          if (d.longDesc.contains("|")){
            d.longDesc.split('\n').toList.foreach(l => {
              val split = l.replace("\r", "").split('|')
              devicesAuxFromComp += Device(
                d.project,
                d.id,
                d.comp,
                d.userId + "." + split(0),
                d.system,
                d.zone,
                d.elemType,
                d.compAbbrev,
                materials.find(_.code == split(1)) match {
                  case Some(value) => value.singleWeight
                  case _ => 0
                },
                split(1),
                d.elemClass,
                "",
                "",
                "",
//                "",
                materials.find(_.code == split(1)) match {
                  case Some(value) => value
                  case _ => Material()
                },
                d.userId + "." + split(0),
                d.userId + "." + split(0),
                split(2),
                split(3).toDoubleOption.getOrElse(0),
                1)
            })
          }
        })
      case _ => List.empty[Device]
    }
    devices ++= devicesAuxFromComp.toList
    devices.filter(_.material.code != "").toList
  }
  def addDeviceToSystem(docNumber: String, stock: String, units: String, count: String, label: String, forLabel: String, addText: String): Unit ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val projectNamesCollection: MongoCollection[ProjectName] = mongo.getCollection("project-names")
        val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[ProjectName] => values.toList
          case _ => List.empty[ProjectName]
        }
        val rkdProject = if (docNumber.contains('-')) docNumber.split('-').head else ""
        val foranProject = if ("""200101-100-10[0-9]""".r.matches(docNumber)) "NT02" else projectNames.find(_.rkd == rkdProject) match {
          case Some(value) => value.foran
          case _ => ""
        }
        val systemDefs = getDeviceSystemDefs(foranProject)
        val system = systemDefs.find(_.descr.contains(docNumber)) match {
          case Some(value) =>
            value.name
          case _ => ""
        }
        val descrs = ListBuffer.empty[SystemLang]
        val newLabel = List(label, stock, units, count, addText).mkString("|")
        if (forLabel == ""){
          DBManager.GetOracleConnection(foranProject) match {
            case Some(oracle) =>
              val s = oracle.createStatement()
              val query = s"select * from systems_lang where system in (select oid from systems where name = '$system')"
              val rs = s.executeQuery(query)
              while (rs.next()){
                descrs +=
                  SystemLang(
                    rs.getInt("system"),
                    rs.getInt("lang"),
                    rs.getString("descr"),
                    Option(rs.getString("long_descr")).getOrElse("")
                  )
              }
              rs.close()
              s.close()
              oracle.close()
            case _ =>
          }

          val longDescr = descrs.find(_.long_descr.length < 900) match {
            case Some(value) => value
            case _ =>
              val nrDescrs = descrs.filter(_.lang > 10).sortBy(_.lang)
              val newNrDescr = if (nrDescrs.nonEmpty){
                SystemLang(nrDescrs.last.systemId, nrDescrs.last.lang + 1, nrDescrs.last.descr, "")
              }
              else{
                SystemLang(descrs.head.systemId, 11, descrs.head.descr, "")
              }
              DBManager.GetOracleConnection(foranProject) match {
                case Some(oracle) =>
                  val s = oracle.createStatement()
                  val query = s"select count(*) as count from foran_language where oid = ${newNrDescr.lang}"
                  val rs = s.executeQuery(query)
                  if (rs.next()){
                    val count = Option(rs.getInt("count")).getOrElse(0)
                    if (count == 0){
                      val oid = newNrDescr.lang
                      val abbrev = "NR" + (oid - 10).toString
                      s.execute(s"insert into foran_language values ($oid, '$abbrev', '$abbrev')")
                    }
                  }
                  rs.close()
                  s.close()
                  oracle.close()
                case _ =>
              }
              DBManager.GetOracleConnection(foranProject) match {
                case Some(oracle) =>
                  val s = oracle.createStatement()
                  val query = s"insert into systems_lang values (${newNrDescr.systemId}, ${newNrDescr.lang}, '${newNrDescr.descr}', '${newNrDescr.long_descr}')"
                  s.execute(query)
                  s.close()
                  oracle.close()
                case _ =>
              }
              newNrDescr
          }

          DBManager.GetOracleConnection(foranProject) match {
            case Some(oracle) =>
              val s = oracle.createStatement()
              val query = s"update systems_lang set long_descr = concat(long_descr, chr(10) || '$newLabel') where system = ${longDescr.systemId} and lang = ${longDescr.lang}"
              s.execute(query)
              s.close()
              oracle.close()
            case _ =>
          }
        }
        else{
          DBManager.GetOracleConnection(foranProject) match {
            case Some(oracle) =>
              val s = oracle.createStatement()
              val query = s"update element_lang set long_descr = concat(long_descr, chr(10) || '$newLabel') where elem in (select oid from v_element_desc where userid = '$forLabel') and lang = -1"
              s.execute(query)
              s.close()
              oracle.close()
            case _ =>
          }
        }
      case _ =>
    }
  }
  def removeDeviceFromSystem(docNumber: String, stock: String, units: String, count: String, label: String, forLabel: String, addText: String): Unit ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val projectNamesCollection: MongoCollection[ProjectName] = mongo.getCollection("project-names")
        val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[ProjectName] => values.toList
          case _ => List.empty[ProjectName]
        }
        val rkdProject = if (docNumber.contains('-')) docNumber.split('-').head else ""
        val foranProject = if ("""200101-100-10[0-9]""".r.matches(docNumber)) "NT02" else projectNames.find(_.rkd == rkdProject) match {
          case Some(value) => value.foran
          case _ => ""
        }
        val systemDefs = getDeviceSystemDefs(foranProject)
        val system = systemDefs.find(_.descr.contains(docNumber)) match {
          case Some(value) =>
            value.name
          case _ => ""
        }
        val descrs = ListBuffer.empty[SystemLang]
        val newLabel = List(label, stock).mkString("|")
        if (forLabel == ""){
          DBManager.GetOracleConnection(foranProject) match {
            case Some(oracle) =>
              val s = oracle.createStatement()
              val query = s"select * from systems_lang where system in (select oid from systems where name = '$system')"
              val rs = s.executeQuery(query)
              while (rs.next()){
                descrs +=
                  SystemLang(
                    rs.getInt("system"),
                    rs.getInt("lang"),
                    rs.getString("descr"),
                    Option(rs.getString("long_descr")).getOrElse("")
                  )
              }
              rs.close()
              s.close()
              oracle.close()
            case _ =>
          }

          descrs.foreach(d => {
            DBManager.GetOracleConnection(foranProject) match {
              case Some(oracle) =>
                val s = oracle.createStatement()
                val qSelect = s"select long_descr from systems_lang where system = ${d.systemId} and lang = ${d.lang}"
                val rs = s.executeQuery(qSelect)
                val labels = if (rs.next()){
                  Option(rs.getString("long_descr")).getOrElse("")
                }
                else{
                  ""
                }
                rs.close()
                if (labels.contains(newLabel)){
                  val newLabelText = labels.split("\n").filter(_.contains("|")).filter(x => !x.contains(newLabel)).mkString("\n")
                  val query = s"update systems_lang set long_descr = '$newLabelText' where system = ${d.systemId} and lang = ${d.lang}"
                  s.execute(query)
                }
                s.close()
                oracle.close()
              case _ =>
            }
          })

          DBManager.GetOracleConnection(foranProject) match {
            case Some(oracle) =>
              val s = oracle.createStatement()
              val query = s"update element_lang set long_descr = replace(long_descr, '${newLabel}', '') where elem in (select oid from v_element_desc where syst_userid = '$system') and lang = -1"
              s.execute(query)
              s.close()
              oracle.close()
            case _ =>
          }




          var removeOid = 0
          val devices = ListBuffer.empty[Device]
          DBManager.GetOracleConnection(foranProject) match {
            case Some(oracle) =>
              val s = oracle.createStatement()
              //val query = s"select * from v_element_desc where syst_userid = '$system'"
              //val query = s"select \n    oid, \n    comp, \n    userid, \n    syst_userid, \n    zone_userid, \n    type, \n    comp_abbrev, \n    weight, \n    stock_code, \n    elem_class,\n    elem_desc1, \n    elem_desc2, \n    stock_code, \n    (select long_descr from COMPONENT_LANG cl where lang = -2 and cl.comp = elemdesc.comp and rownum = 1) as long_desc\nfrom \n    v_element_desc  elemdesc\nwhere \n    syst_userid = '$system'"
              val query = s"select \n    oid, \n    comp, \n    userid, \n    syst_userid, \n    zone_userid, \n    type, \n    comp_abbrev, \n    weight, \n    stock_code, \n    elem_class,\n    elem_desc1, \n    elem_desc2, \n    stock_code, \n    (select long_descr from COMPONENT_LANG cl where lang = -2 and cl.comp = elemdesc.comp and rownum = 1) as long_desc,\n    (select long_descr from element_lang cl where lang = -2 and cl.elem = elemdesc.oid and rownum = 1) as long_desc_elem\nfrom \n    v_element_desc  elemdesc\nwhere \n    syst_userid = '$system'"
              val rs = s.executeQuery(query)
              while (rs.next()) {
                devices += Device(
                  foranProject,
                  Option(rs.getInt("OID")).getOrElse(-1),
                  Option(rs.getInt("COMP")).getOrElse(-1),
                  Option(rs.getString("USERID")).getOrElse(""),
                  Option(rs.getString("SYST_USERID")).getOrElse(""),
                  Option(rs.getString("ZONE_USERID")).getOrElse(""),
                  Option(rs.getString("TYPE")).getOrElse(""),
                  Option(rs.getString("COMP_ABBREV")).getOrElse(""),
                  Option(rs.getDouble("WEIGHT")).getOrElse(-1),
                  Option(rs.getString("STOCK_CODE")).getOrElse(""),
                  Option(rs.getInt("ELEM_CLASS")).getOrElse(-1),
                  Option(rs.getString("ELEM_DESC1")).getOrElse(""),
                  Option(rs.getString("ELEM_DESC2")).getOrElse(""),
                  Option(rs.getString("LONG_DESC")).getOrElse(""),
//                  Option(rs.getString("LONG_DESC_ELEM")).getOrElse(""),
                  Material(),
                  Option(rs.getString("USERID")).getOrElse(""),
                  "")
              }
              rs.close()
              s.close()
              oracle.close()
            case _ => List.empty[Device]
          }
          devices.foreach(d => {
            d.longDesc.split('\n').foreach(l => {
              if (system + '.' + l == newLabel){
                removeOid = d.comp
              }
            })
          })

          DBManager.GetOracleConnection(foranProject) match {
            case Some(oracle) =>
              val s = oracle.createStatement()
              val query = s"update component_lang set long_descr = replace(long_descr, '${newLabel.replace(system + ".", "")}', '') where comp = $removeOid and lang = -2"
              s.execute(query)
              s.close()
              oracle.close()
            case _ =>
          }
        }
        else{
          DBManager.GetOracleConnection(foranProject) match {
            case Some(oracle) =>
              val s = oracle.createStatement()
              val query = s"update element_lang set long_descr = replace(long_descr, '$newLabel', '') where elem in (select oid from v_element_desc where userid = '$forLabel') and lang = -1"
              s.execute(query)
              s.close()
              oracle.close()
            case _ =>
          }
        }
      case _ =>
    }
  }
  def getSystemName(docNumber: String): String ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val projectNamesCollection: MongoCollection[ProjectName] = mongo.getCollection("project-names")
        val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[ProjectName] => values.toList
          case _ => List.empty[ProjectName]
        }
        val rkdProject = if (docNumber.contains('-')) docNumber.split('-').head else ""
        val foranProject = if ("""200101-100-10[0-9]""".r.matches(docNumber)) "NT02" else projectNames.find(_.rkd == rkdProject) match {
          case Some(value) => value.foran
          case _ => ""
        }
        val systemDefs = getDeviceSystemDefs(foranProject)
        systemDefs.find(_.descr.contains(docNumber)) match {
          case Some(value) =>
            value.descr.replace(docNumber, "").trim
          case _ => ""
        }
      case _ =>  ""
    }
  }
  def getUnits: List[Units] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val units: MongoCollection[Units] = mongo.getCollection("materials-n-units")
        Await.result(units.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[Units] =>
            values.toList
          case _ => List.empty[Units]
        }
      case _ => List.empty[Units]
    }
  }
  def getDevicesWithAccommodations(docNumber: String): List[Device] = {
    val devices = getDevices(docNumber) ++ getAccommodationsAsDevices(docNumber, "ru")
    devices.filter(_.desc2.contains("&")).foreach(d => {
      val ids = d.desc2.split("&")
      val accom = devices.filter(_.elemType == "accommodation").filter(x => ids.contains(x.userId) && x.zone == d.zone)
      accom.foreach(x => x.userId = d.userId + "." + x.userId)
    })
    val userIdsReplace = getAccommodationUserIds(docNumber)
    devices.filter(_.elemType == "accommodation").foreach(x => {
      userIdsReplace.find(_.userId == x.userId) match {
        case Some(value) => x.userId = value.userIdNew
        case _ => None
      }
    })
    devices.sortBy(_.userId)
  }
  def getProjectFromDocNumber(docNumber: String): (String, String) = {
    DBManager.GetMongoConnection() match {
      case Some(mongoData) =>
        val projectNamesCollection: MongoCollection[ProjectName] = mongoData.getCollection("project-names")
        val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[ProjectName] => values.toList
          case _ => List.empty[ProjectName]
        }
        """\d{6}(?=-\d{3}\w{0,1}-\d{3,4})""".r.findFirstIn(docNumber) match {
          case Some(rkdProject) =>
            projectNames.find(_.rkd == rkdProject) match {
              case Some(project) =>
                val systems = getDeviceSystemDefs(project.foran)
                systems.find(x => x.descr.contains(docNumber)) match {
                  case Some(system) =>
                    (project.foran, system.name)
                  case _ => (project.foran, "")
                }
              case _ => ("", "")
            }
          case _ => ("", "")
        }
    }
  }
  def getDeviceSystemDefs(project: String): List[SystemDef] = {
    val systemDefs = ListBuffer.empty[SystemDef]
    DBManager.GetOracleConnection(project) match {
      case Some(oracleConnection) =>
        val stmt = oracleConnection.createStatement()
        val query = "SELECT S.NAME AS NAME, L.DESCR AS DESCR FROM SYSTEMS S, SYSTEMS_LANG L WHERE S.OID = L.SYSTEM AND L.LANG = -2"
        val rs = stmt.executeQuery(query)
        while (rs.next()) {
          systemDefs += SystemDef(project, rs.getString("NAME") match {
            case value: String => value
            case _ => ""
          }, rs.getString("DESCR") match {
            case value: String => value
            case _ => ""
          })
        }
        rs.close()
        stmt.close()
        rs.close()
        oracleConnection.close()
      case _ =>
    }
    systemDefs.toList
  }
}
