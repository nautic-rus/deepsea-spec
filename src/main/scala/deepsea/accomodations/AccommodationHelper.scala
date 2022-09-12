package deepsea.accomodations

import deepsea.accomodations.AccommodationManager.{Accommodation, AccommodationAux}
import deepsea.database.DBManager
import deepsea.devices.DeviceManager.{Device, DeviceAux}
import deepsea.pipe.PipeManager.{Material, ProjectName, SystemDef}
import org.mongodb.scala.{MongoCollection, classTagToClassOf}
import org.mongodb.scala.model.Filters.equal

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source

trait AccommodationHelper {
  def getAccommodations(docNumber: String): List[Accommodation] ={
    val accommodations = ListBuffer.empty[Accommodation]
    //val devicesAux = ListBuffer.empty[AccommodationAux]
    //val devicesAuxFromSystem = ListBuffer.empty[AccommodationAux]
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
        val docNumberSuffix = docNumber.split('-').drop(1).mkString("-")
        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracle) =>
            val s = oracle.createStatement()
            val query = Source.fromResource("queries/accommodations.sql").mkString.replaceAll("&docNumberSuffix", docNumberSuffix)
            val rs = s.executeQuery(query)
            while (rs.next()) {
              accommodations += Accommodation(
                foranProject,
                Option(rs.getInt("MOD_OID")).getOrElse(-1),
                Option(rs.getInt("AS_OID")).getOrElse(-1),
                Option(rs.getDouble("WEIGHT")).getOrElse(0),
                Option(rs.getDouble("SURFACE")).getOrElse(0),
                Option(rs.getString("USERID")).getOrElse(""),
                Option(rs.getString("MATERIAL")).getOrElse(""),
                Option(rs.getString("MATERIAL_DESCRIPTION")).getOrElse(""),
                Option(rs.getDouble("BS_WEIGHT")).getOrElse(0),
                Option(rs.getString("ZONE")).getOrElse(""),
                Option(rs.getString("MATERIAL_DESCRIPTION")) match {
                  case Some(descr) =>
                    if (descr.contains("#")){
                      val code = descr.split("#").last
                      materials.find(_.code == code) match {
                        case Some(value) => value
                        case _ => Material()
                      }
                    }
                    else {
                      Material()
                    }
                  case _ => Material()
                })
            }
            s.close()
            oracle.close()
          case _ => List.empty[Accommodation]
        }
      case _ => List.empty[Accommodation]
    }
    accommodations.toList
  }
  def addAccommodationToSystem(docNumber: String, stock: String, units: String, count: String, label: String, forLabel: String = ""): Unit ={
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
  def getASName(docNumber: String): String ={
    var res = ""
    val docNumberSuffix = docNumber.split('-').drop(1).mkString("-")
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
        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracleConnection) =>
            val stmt = oracleConnection.createStatement()
            val query = s"SELECT DESCR FROM AS_LANG WHERE OID IN (SELECT OID FROM AS_LIST WHERE USERID = '$docNumberSuffix') AND DESCR IS NOT NULL AND ROWNUM = 1"
            val rs = stmt.executeQuery(query)
            if (rs.next()){
             res = rs.getString("DESCR") match {
                case value: String => value
                case _ => ""
              }
            }
            stmt.close()
            rs.close()
            oracleConnection.close()
            res
          case _ => res
        }

      case _ => res
    }
  }
}
