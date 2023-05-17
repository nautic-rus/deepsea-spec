package deepsea.accomodations

import deepsea.accomodations.AccommodationManager.{Accommodation, AccommodationAux, AccommodationGroup, BBox, Zone}
import deepsea.database.DatabaseManager.RsIterator
import deepsea.database.{DBManager, DatabaseManager}
import deepsea.devices.DeviceManager.{Device, DeviceAux}
import deepsea.pipe.PipeManager.{Material, ProjectName, SystemDef, Units}
import local.pdf.en.accom.AccomReportEn.getSystemDefs
import org.mongodb.scala.{MongoCollection, classTagToClassOf}
import org.mongodb.scala.model.Filters.equal

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source

trait AccommodationHelper {
//  def getAccommodations(docNumber: String): List[Accommodation] ={
//    val accommodations = ListBuffer.empty[Accommodation]
//    //val devicesAux = ListBuffer.empty[AccommodationAux]
//    //val devicesAuxFromSystem = ListBuffer.empty[AccommodationAux]
//    DBManager.GetMongoConnection() match {
//      case Some(mongo) =>
//        val materialsNCollectionName = "materials-n"
//        val materialsCollection: MongoCollection[Material] = mongo.getCollection(materialsNCollectionName)
//        val projectNamesCollection: MongoCollection[ProjectName] = mongo.getCollection("project-names")
//        val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
//          case values: Seq[ProjectName] => values.toList
//          case _ => List.empty[ProjectName]
//        }
//        val rkdProject = if (docNumber.contains('-')) docNumber.split('-').head else ""
//        val foranProject = projectNames.find(_.rkd == rkdProject) match {
//          case Some(value) => value.foran
//          case _ => ""
//        }
//        val materials = Await.result(materialsCollection.find(equal("projects", rkdProject)).toFuture(), Duration(30, SECONDS)) match {
//          case values: Seq[Material] => values.toList
//          case _ => List.empty[Material]
//        }
//        val docNumberSuffix = docNumber.split('-').drop(1).mkString("-")
//        val zones = getZones(foranProject)
//        DBManager.GetOracleConnection(foranProject) match {
//          case Some(oracle) =>
//            val s = oracle.createStatement()
//            val query = Source.fromResource("queries/accommodations.sql").mkString.replaceAll("&docNumberSuffix", docNumberSuffix)
//            val rs = s.executeQuery(query)
//            while (rs.next()) {
//              val zone: String = Option(rs.getString("ZONE")).getOrElse("")
//              val weight: Double = Option(rs.getDouble("WEIGHT")).getOrElse(0)
//              val surface: Double = Option(rs.getDouble("SURFACE")).getOrElse(0)
//              val bsWeight: Double = Option(rs.getDouble("BS_WEIGHT")).getOrElse(0)
//              val userId: String = Option(rs.getString("USERID")).getOrElse("")
//              val profileStock: String = Option(rs.getString("PROFILE_STOCK")).getOrElse("")
//              val plateStock: String = Option(rs.getString("PLATE_STOCK")).getOrElse("")
//              val bBox = BBox(
//                Option(rs.getDouble("X_MIN")).getOrElse(0),
//                Option(rs.getDouble("Y_MIN")).getOrElse(0),
//                Option(rs.getDouble("Z_MIN")).getOrElse(0),
//                Option(rs.getDouble("X_MAX")).getOrElse(0),
//                Option(rs.getDouble("Y_MAX")).getOrElse(0),
//                Option(rs.getDouble("Z_MAX")).getOrElse(0)
//              )
//              accommodations += Accommodation(
//                foranProject,
//                Option(rs.getInt("MOD_OID")).getOrElse(-1),
//                Option(rs.getInt("AS_OID")).getOrElse(-1),
//                weight,
//                surface,
//                (accommodations.length + 1).toString,
//                Option(rs.getString("MATERIAL")).getOrElse("") + Option(rs.getString("PROFILE_MATERIAL")).getOrElse("") + Option(rs.getString("PLATE_MATERIAL")).getOrElse(""),
//                Option(rs.getString("MATERIAL_DESCRIPTION")).getOrElse(""),
//                bsWeight,
//                zones.filter(x => bBoxIntersects(x.BBox, bBox)).map(_.name).mkString(","),
//                profileStock,
//                plateStock,
//                Option(rs.getString("MATERIAL_DESCRIPTION")) match {
//                  case Some(descr) =>
//                    if (descr.contains("#")){
//                      val code = descr.split("#").last
//                      materials.find(_.code == code) match {
//                        case Some(value) => value
//                        case _ => Material()
//                      }
//                    }
//                    else {
//                      materials.find(x => x.code == profileStock || x.code == plateStock) match {
//                        case Some(value) => value
//                        case _ => Material()
//                      }
//                    }
//                  case _ => Material()
//                })
//            }
//            s.close()
//            oracle.close()
//          case _ => List.empty[Accommodation]
//        }
//      case _ => List.empty[Accommodation]
//    }
//    accommodations.toList
//  }
  def getAccommodationsAsDevices(docNumber: String, lang: String): List[Device] ={
    val accommodations = ListBuffer.empty[Accommodation]
    val groups = ListBuffer.empty[AccommodationGroup]
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
        val docNumberSuffix = docNumber.split('-').drop(1).mkString("-")
        val zones = getZones(foranProject)
        var counter = 0

        val systemDefs = getSystemDefs(foranProject)
        val system = systemDefs.find(_.descr.contains(docNumber)) match {
          case Some(value) =>
            value.name
          case _ => ""
        }

        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracle) =>
            val s = oracle.createStatement()
            val query = s"select system, long_descr from systems_lang where system in (select oid from systems where name = '$system')"
            val rs = s.executeQuery(query)
            while (rs.next()) {
              val descr = Option(rs.getString("LONG_DESCR")).getOrElse("").replace("\r", "")
              if (descr.contains("|")){
                descr.split('\n').foreach(l => {
                  if (l.contains('@')){
                    val split = l.replace("@", "").split('|')
                    if (split.length > 2){
                      groups += AccommodationGroup(split.head, split(1) + split(2))
                    }
                  }
                })
              }
            }
            s.close()
            oracle.close()
          case _ => List.empty[Device]
        }

        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracle) =>
            val s = oracle.createStatement()
            val query = Source.fromResource("queries/accommodations.sql").mkString.replaceAll("&docNumberSuffix", docNumberSuffix)
            val rs = s.executeQuery(query)
            while (rs.next()) {
              val zone: String = Option(rs.getString("ZONE")).getOrElse("")
              val weight: Double = Option(rs.getDouble("WEIGHT")).getOrElse(0)
              val surface: Double = Option(rs.getDouble("SURFACE")).getOrElse(0)
              val bsWeight: Double = Option(rs.getDouble("BS_WEIGHT")).getOrElse(0)
              val userId: String = Option(rs.getString("USERID")).getOrElse("")
              val profileStock: String = Option(rs.getString("PROFILE_STOCK")).getOrElse("")
              val plateStock: String = Option(rs.getString("PLATE_STOCK")).getOrElse("")
              val profileLength: Double = Option(rs.getDouble("PROFILE_LENGTH")).getOrElse(0)
              val profileSection: Int = Option(rs.getInt("PROFILE_SECTION")).getOrElse(0)
              val norm: String = Option(rs.getString("NORM")).getOrElse("")
              val normDescr: String = Option(rs.getString("NORM_DESCR")).getOrElse("")
              val materialDescription: String = Option(rs.getString("MATERIAL_DESCRIPTION")).getOrElse("")
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
                weight,
                surface,
                userId,
                Option(rs.getString("MATERIAL")).getOrElse("") + Option(rs.getString("PROFILE_MATERIAL")).getOrElse("") + Option(rs.getString("PLATE_MATERIAL")).getOrElse(""),
                Option(rs.getString("MATERIAL_DESCRIPTION")).getOrElse(""),
                Option(rs.getInt("OBJ_TYPE")).getOrElse(-1),
                List(
                  Option(rs.getDouble("PAR1")).getOrElse(0),
                  Option(rs.getDouble("PAR2")).getOrElse(0),
                  Option(rs.getDouble("PAR3")).getOrElse(0),
                  Option(rs.getDouble("PAR4")).getOrElse(0),
                  Option(rs.getDouble("PAR5")).getOrElse(0),
                  Option(rs.getDouble("PAR6")).getOrElse(0),
                  Option(rs.getDouble("PAR7")).getOrElse(0),
                  Option(rs.getDouble("PAR8")).getOrElse(0),
                ),
                bsWeight,
                zone,
//                zones.filter(x => bBoxIntersects(x.BBox, bBox)).map(_.name).mkString(","),
                profileStock,
                plateStock,
                if (normDescr != ""){
                  materials.find(_.code == normDescr) match {
                    case Some(value) => value.copy(name = value.name)
                    case _ => Material()
                  }
                }
                else{
                  if (materialDescription != ""){
                    if (materialDescription.contains("#")){
                      val code = materialDescription.split("#").last
                      materials.find(_.code == code) match {
                        case Some(value) => value
                        case _ => Material()
                      }
                    }
                    else{
                      Material()
                    }
                  }
                  else{
                    materials.find(x => x.code == profileStock || x.code == plateStock) match {
                      case Some(value) => value
                      case _ => Material()
                    }
                  }
                },
                profileLength,
                profileSection)
            }
            s.close()
            oracle.close()
          case _ => List.empty[Accommodation]
        }
      case _ => List.empty[Accommodation]
    }

    val res = accommodations.map(_.asDevice).filter(m => m.material.code != "" && !groups.map(_.code).contains(m.material.code + m.zone) && !groups.map(_.code).contains(m.material.code)).tapEach(x => {
      x.units = x.material.units
      if (x.units == x.material.units && x.units == "796"){
        x.weight = x.material.singleWeight
      }
      else if (x.material.units == "055"){

      }
    }).toList ++
    accommodations.map(_.asDevice).filter(m => m.material.code != "" && groups.map(x => x.code).contains(m.material.code + m.zone)).groupBy(x => x.material.code + x.material.name + x.zone).map(acc => {
      acc._2.head.copy(weight = acc._2.map(_.weight).sum, count = acc._2.head.units match {
        case "006" => acc._2.head.count
        case "055" => acc._2.head.material.singleWeight
        case _ => acc._2.map(_.count).sum
      }, userId = groups.find(x => x.code == acc._2.head.material.code + acc._2.head.zone) match {
        case Some(group) => group.userId
        case _ => "NoUserId"
      }, material = if (acc._2.head.material.name.contains("L=")){
        acc._2.head.material.copy(name = acc._2.head.material.name + ", " + acc._2.length + "pcs",
          translations = acc._2.head.material.translations.map(t => t.copy(name = t.name + ", " + acc._2.length + "шт")))
      }
      else{
        acc._2.head.material
      })
    }).toList ++
    accommodations.map(_.asDevice).filter(m => m.material.code != "" && groups.map(_.code).contains(m.material.code) && !groups.map(_.code).contains(m.material.code + m.zone)).groupBy(x => x.material.code + x.material.name).map(acc => {
      acc._2.head.copy(weight = acc._2.map(_.weight).sum, count = acc._2.head.units match {
        case "006" => acc._2.head.count
        case "055" => acc._2.head.material.singleWeight
        case _ => acc._2.map(_.count).sum
      }, userId = groups.find(x => acc._1.startsWith(x.code)) match {
        case Some(group) => group.userId
        case _ => "NoUserId"
      }, material = if (acc._2.head.material.name.contains("L=")){
        acc._2.head.material.copy(name = acc._2.head.material.name + ", " + acc._2.length + "pcs",
          translations = acc._2.head.material.translations.map(t => t.copy(name = t.name + ", " + acc._2.length + "шт")))
      }
      else{
        acc._2.head.material
      })
    })

    val userIds = ListBuffer.empty[String]
    res.sortBy(x =>
      if (x.material.name.contains("L=")){
        val l = "(?<=L=)\\d+".r.findFirstIn(x.material.name) match {
          case Some(value) => value
          case _ => ""
        }
        "B" + addLeftZeros(l, 10)
      }
      else if (x.userId.contains(".")) {
        "C" + addLeftZeros(x.userId.split("\\.").head) + addLeftZeros(x.userId.split("\\.").last)
      }
      else {
        "A" + addLeftZeros(x.userId)
      }).foreach(x => {
      val userId = x.userId
      if (userIds.contains(x.userId)){
        x.userId = x.userId + "." + (userIds.count(y => y == x.userId)).toString + "*"
      }
      userIds += userId
    })
    res.tapEach(x => x.units = x.material.units).filter(_.material.code != "").toList
  }
  def addLeftZeros(input: String, length: Int = 5): String ={
    var res = input
    while (res.length < length){
      res = "0" + res
    }
    res
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
  def addGroupToSystem(docNumber: String, stock: String, userId: String): Unit ={
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
        val systemDefs = getSystemDefs(foranProject)
        val system = systemDefs.find(_.descr.contains(docNumber)) match {
          case Some(value) =>
            value.name
          case _ => ""
        }
        val newLabel = '@' + List(userId, stock).mkString("|")
        DBManager.GetOracleConnection(foranProject) match {
          case Some(oracle) =>
            val s = oracle.createStatement()
            val query = s"update systems_lang set long_descr = concat(long_descr, chr(10) || '$newLabel') where system in (select oid from systems where name = '$system')"
            s.execute(query)
            s.close()
            oracle.close()
          case _ =>
        }
      case _ =>
    }
  }

  def setAccommodationLabel(docNumber: String, label: String, oid: Int): String = {
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
          case Some(oracle) =>
            val s = oracle.createStatement()
            val query = s"update as_elem set userid = '$label' where oid = $oid"
            s.execute(query)
            s.close()
            oracle.close()
          case _ =>
        }
      case _ =>
    }
    "success"
  }

}
