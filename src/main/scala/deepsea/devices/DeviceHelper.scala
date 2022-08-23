package deepsea.devices

import deepsea.database.DBManager
import deepsea.database.DatabaseManager.GetOracleConnection
import deepsea.devices.DeviceManager.{Device, DeviceAux}
import deepsea.pipe.PipeManager.{Material, PipeSeg, ProjectName, SystemDef}
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.{and, equal, notEqual}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait DeviceHelper{

  def getDevices(docNumber: String): List[Device] ={
    val devices = ListBuffer.empty[Device]
    val devicesAux = ListBuffer.empty[DeviceAux]
    val devicesAuxFromSystem = ListBuffer.empty[DeviceAux]
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
        val foranProject = projectNames.find(_.rkd == rkdProject) match {
          case Some(value) => value.foran
          case _ => ""
        }
        val materials = Await.result(materialsCollection.find(equal("projects", rkdProject)).toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[Material] => values.toList
          case _ => List.empty[Material]
        }
        val systemDefs = getSystemDefs(foranProject)
        val system = systemDefs.find(_.descr.contains(docNumber)) match {
          case Some(value) =>
            value.name
          case _ => ""
        }
        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracle) =>
            val s = oracle.createStatement()
            val query = s"select * from v_element_desc where syst_userid = '$system'"
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
                materials.find(_.code == Option(rs.getString("STOCK_CODE")).getOrElse("")) match {
                  case Some(value) => value
                  case _ => Material()
                },
                Option(rs.getString("USERID")).getOrElse(""),
                "")
            }
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
                Option(rs.getString("LONG_DESCR")).getOrElse(""),
              )
            }
            s.close()
            oracle.close()
          case _ => List.empty[Device]
        }
        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracle) =>
            val s = oracle.createStatement()
            val query = s"select system, descr from systems_lang where system in (select oid from systems where name = '$system')"
            val rs = s.executeQuery(query)
            while (rs.next()) {
              devicesAux += DeviceAux(
                Option(rs.getInt("SYSTEM")).getOrElse(-1),
                Option(rs.getString("DESCR")).getOrElse(""),
              )
            }
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
                    deviceBase.userId + '.' + split(0),
                    deviceBase.system,
                    deviceBase.zone,
                    deviceBase.elemType,
                    deviceBase.compAbbrev,
                    deviceBase.weight,
                    split(1),
                    deviceBase.elemClass,
                    "",
                    "",
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
                "",
                "",
                "",
                0,
                split(1),
                0,
                "",
                "",
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
      case _ => List.empty[Device]
    }
    devices.toList
  }
  def addDeviceToSystem(docNumber: String, stock: String, units: String, count: String, label: String, forLabel: String = ""): Unit ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val projectNamesCollection: MongoCollection[ProjectName] = mongo.getCollection("project-names")
        val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[ProjectName] => values.toList
          case _ => List.empty[ProjectName]
        }
        val rkdProject = if (docNumber.contains('-')) docNumber.split('-').head else ""
        val foranProject = projectNames.find(_.rkd == rkdProject) match {
          case Some(value) => value.foran
          case _ => ""
        }
        val systemDefs = getSystemDefs(foranProject)
        val system = systemDefs.find(_.descr.contains(docNumber)) match {
          case Some(value) =>
            value.name
          case _ => ""
        }
        val newLabel = List(label, stock, units, count).mkString("|")
        if (forLabel == ""){
          DBManager.GetOracleConnection(foranProject) match {
            case Some(oracle) =>
              val s = oracle.createStatement()
              val query = s"update systems_lang set long_descr = concat(long_descr, chr(10) || '$newLabel') where system in (select oid from systems where name = '$system')"
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
  def getSystemName(docNumber: String): String ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val projectNamesCollection: MongoCollection[ProjectName] = mongo.getCollection("project-names")
        val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[ProjectName] => values.toList
          case _ => List.empty[ProjectName]
        }
        val rkdProject = if (docNumber.contains('-')) docNumber.split('-').head else ""
        val foranProject = projectNames.find(_.rkd == rkdProject) match {
          case Some(value) => value.foran
          case _ => ""
        }
        val systemDefs = getSystemDefs(foranProject)
        systemDefs.find(_.descr.contains(docNumber)) match {
          case Some(value) =>
            value.descr.replace(docNumber, "").trim
          case _ => ""
        }
      case _ =>  ""
    }
  }
  def getSystemDefs(project: String): List[SystemDef] ={
    val systemDefs = ListBuffer.empty[SystemDef]
    DBManager.GetOracleConnection(project) match {
      case Some(oracleConnection) =>
        val stmt = oracleConnection.createStatement()
        val query = "SELECT S.NAME AS NAME, L.DESCR AS DESCR FROM SYSTEMS S, SYSTEMS_LANG L WHERE S.OID = L.SYSTEM AND L.LANG = -2"
        val rs = stmt.executeQuery(query)
        while (rs.next()){
          systemDefs += SystemDef(project, rs.getString("NAME") match {
            case value: String => value
            case _ => ""
          }, rs.getString("DESCR") match {
            case value: String => value
            case _ => ""
          })
        }
        stmt.close()
        rs.close()
        oracleConnection.close()
      case _ =>
    }
    systemDefs.toList
  }

}
