package deepsea.accomodations

import deepsea.accomodations.AccommodationManager.{Accommodation, AccommodationAux, BBox, Zone}
import deepsea.database.DatabaseManager.RsIterator
import deepsea.database.{DBManager, DatabaseManager}
import deepsea.devices.DeviceManager.{Device, DeviceAux}
import deepsea.pipe.PipeManager.{Material, ProjectName, SystemDef, Units}
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
        val zones = getZones(foranProject)
        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracle) =>
            val s = oracle.createStatement()
            val query = Source.fromResource("queries/accommodations.sql").mkString.replaceAll("&docNumberSuffix", docNumberSuffix)
            val rs = s.executeQuery(query)
            while (rs.next()) {
              val zone = Option(rs.getString("ZONE")).getOrElse("")
              val bBox = BBox(
                Option(rs.getDouble("X_MIN")).getOrElse(0),
                Option(rs.getDouble("Y_MIN")).getOrElse(0),
                Option(rs.getDouble("Z_MIN")).getOrElse(0),
                Option(rs.getDouble("X_MAX")).getOrElse(0),
                Option(rs.getDouble("Y_MAX")).getOrElse(0),
                Option(rs.getDouble("Z_MAX")).getOrElse(0)
              )
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
                zones.filter(x => bBoxIntersects(x.BBox, bBox)).map(_.name).mkString(","),
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
  def getASName(docNumber: String): String ={
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
            val descr = if (rs.next()){
             rs.getString("DESCR") match {
                case value: String => value
                case _ => ""
              }
            }
            else{
              ""
            }
            stmt.close()
            rs.close()
            oracleConnection.close()
            descr
          case _ => ""
        }
      case _ => ""
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
  def bBoxIntersects(a: BBox, b: BBox): Boolean = {
      a.xMin <= b.xMax &&
      a.xMax >= b.xMin &&
      a.yMin <= b.yMax &&
      a.yMax >= b.yMin &&
      a.zMin <= b.zMax &&
      a.zMax >= b.zMin
  }
  def getZones(foranProject: String): List[Zone] ={
    DBManager.GetOracleConnection(foranProject) match {
      case Some(oracleConnection) =>
        val stmt = oracleConnection.createStatement()
        val query = s"SELECT * FROM ZONE"
        val rSet = stmt.executeQuery(query)
        val zones = RsIterator(rSet).map(rs => {
          Zone(
            Option(rs.getString("NAME")).getOrElse(""),
            BBox(
              Option(rs.getDouble("XMIN")).getOrElse(0),
              Option(rs.getDouble("YMIN")).getOrElse(0),
              Option(rs.getDouble("ZMIN")).getOrElse(0),
              Option(rs.getDouble("XMAX")).getOrElse(0),
              Option(rs.getDouble("YMAX")).getOrElse(0),
              Option(rs.getDouble("ZMAX")).getOrElse(0)
            )
          )
        }).toList
        stmt.close()
        rSet.close()
        oracleConnection.close()
        zones
      case _ => List.empty[Zone]
    }
  }
}
