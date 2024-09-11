package deepsea.pipe

import deepsea.pipe.PipeManager.{ElecEquip, GetPipeSegs, GetPipeSegsBilling, GetPipeSegsByDocNumber, GetSpoolLocks, GetSystems, GetZones, Material, MaterialQuality, PipeLineSegment, PipeSeg, PipeSegActual, PipeSegBilling, PipeSegExtended, PipeSup, Pls, PlsElem, PlsParam, ProjectName, SetSpoolLock, SpoolLock, SystemDef, Units, UpdatePipeComp, UpdatePipeJoints}
import org.mongodb.scala.{Document, MongoClient, MongoCollection, MongoDatabase, bson}
import org.mongodb.scala.model.Filters.{and, equal, notEqual}
import akka.http.scaladsl.{Http, HttpExt}
import akka.pattern.ask
import akka.util.Timeout
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.actors.ActorManager
import deepsea.database.DBManager
import deepsea.database.DBManager._
import deepsea.devices.DeviceManager.{Device, DeviceAux}
import deepsea.files.FileManager.GenerateUrl
import deepsea.materials.MaterialsHelper
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.Filters.{all, and, equal, in, notEqual}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import org.mongodb.scala.model.Filters
import io.circe.parser.decode
import local.pdf.ru.ele.EleTrayCableBoxReportRu.getIssueProjects

import java.io.{File, FileInputStream, FileOutputStream}
import java.net.{URL, URLEncoder}
import java.nio.file.{Files, StandardCopyOption}
import java.sql.Connection
import java.util.Date
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.reflect.io.Directory

trait PipeHelper extends Codecs with MaterialsHelper {
  def getSystems(project: String): List[SystemDef] = {
    GetMongoCacheConnection() match {
      case Some(mongo) =>

        val vPipeCompCollectionActual = "vPipeCompActual"

        val vPipeCompActual: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeCompCollectionActual)

        Await.result(vPipeCompActual.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[PipeSegActual] =>
            Await.result(mongo.getCollection(values.last.name).distinct("system", equal("project", project)).toFuture(), Duration(30, SECONDS)) match {
              case systems: Seq[String] =>
                getSystemDefs(project).filter(x => systems.contains(x.name))
              case _ => List.empty[SystemDef]
            }
          case _ => List.empty[SystemDef]
        }
    }
  }
  def getZones(project: String): List[String] = {
    GetMongoCacheConnection() match {
      case Some(mongo) =>

        val vPipeCompCollectionActual = "vPipeCompActual"

        val vPipeCompActual: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeCompCollectionActual)

        Await.result(vPipeCompActual.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[PipeSegActual] =>
            Await.result(mongo.getCollection(values.last.name).distinct("zone", equal("project", project)).toFuture(), Duration(30, SECONDS)) match {
              case systems: Seq[String] =>
                systems.toList
              case _ => List.empty[String]
            }
          case _ => List.empty[String]
        }
    }
  }
  def getPipeSegs(project: String, system: String = "", sqInSystem: Int = -1): List[PipeSeg] ={
    val pipeSegs = DBManager.GetMongoCacheConnection() match {
      case Some(mongo) =>
        val vPipeCompCollectionActualName = "vPipeCompActual"
        val vPipeCompActualCollection: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeCompCollectionActualName)
        val vPipeJointsCollectionActualName = "vPipeJointsActual"
        val vPipeJointsActualCollection: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeJointsCollectionActualName)
        DBManager.GetMongoConnection() match {
          case Some(mongoData) =>
            val materialsNCollectionName = "materials-n"
            val materialsCollection: MongoCollection[Material] = mongoData.getCollection(materialsNCollectionName)
            val projectNamesCollection: MongoCollection[ProjectName] = mongoData.getCollection("project-names")
            val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[ProjectName] => values.toList
              case _ => List.empty[ProjectName]
            }
            val rkdProject = projectNames.find(_.foran == project) match {
              case Some(value) => value.rkd
              case _ => ""
            }
//            val materials = Await.result(materialsCollection.find(equal("project", rkdProject)).toFuture(), Duration(30, SECONDS)) match {
//              case values: Seq[Material] => values.toList
//              case _ => List.empty[Material]
//            }
            val materials = getMaterials.filter(_.project == rkdProject)
            val systemDefs = getSystemDefs(project)
            val res = ListBuffer.empty[PipeSeg]
            Await.result(vPipeCompActualCollection.find().toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[PipeSegActual] =>
                Await.result(mongo.getCollection[PipeSeg](values.last.name).find(and(equal("project", project), if (system != "") equal("system", system) else notEqual("system", system), if (sqInSystem != -1) equal("sqInSystem", sqInSystem) else notEqual("sqInSystem", sqInSystem))).toFuture(), Duration(300, SECONDS)) match {
                  case pipeSegs: Seq[PipeSeg] =>
                    pipeSegs.foreach(x => x.material = materials.find(_.code == x.stock) match {
                      case Some(value) => value
                      case _ => Material()
                    })
                    pipeSegs.foreach(p => p.systemDescr = systemDefs.find(_.name == p.system) match {
                      case Some(systemDef) => systemDef.descr
                      case _ => ""
                    })
                    res ++= pipeSegs.filter(_.typeCode != "GASK")
                  case _ =>
                }
              case _ =>
            }

            val valves = ListBuffer.empty[PipeSeg]
            res.foreach(pipeSeg => {
              """\w{12}\d{4}""".r.findFirstIn(pipeSeg.classDescription) match {
                case Some(value) =>
                  materials.find(_.code == value) match {
                    case Some(addMaterial) =>
                      valves += pipeSeg.copy(material = addMaterial, stock = addMaterial.code, weight = addMaterial.singleWeight, compUserId = addMaterial.name, spPieceId = res.filter(_.spool == pipeSeg.spool).maxBy(_.spPieceId).spPieceId + 1)
                    case _ => None
                  }
                case _ => None
              }
            })

            res ++= valves


//            if (res.isEmpty){
//              res ++= getHvacSegs(project, system, sqInSystem)
//            }

            res ++= getHvacSegs(project, system, sqInSystem)

            if (sqInSystem == -1){
              //res ++= getElecEquips(project, system)
            }

            Await.result(vPipeJointsActualCollection.find().toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[PipeSegActual] =>
                Await.result(mongo.getCollection[PipeSeg](values.last.name).find(and(equal("project", project), if (system != "") equal("system", system) else notEqual("system", system), if (sqInSystem != -1) equal("sqInSystem", sqInSystem) else notEqual("sqInSystem", sqInSystem))).toFuture(), Duration(300, SECONDS)) match {
                  case pipeSegs: Seq[PipeSeg] =>
                    pipeSegs.foreach(x => x.material = materials.find(_.code == x.stock) match {
                      case Some(value) => value
                      case _ => Material()
                    })
                    pipeSegs.foreach(p => p.systemDescr = systemDefs.find(_.name == p.system) match {
                      case Some(systemDef) => systemDef.descr
                      case _ => ""
                    })
                    res ++= pipeSegs.filter(_.stock != "")
                  case _ =>
                }
              case _ =>
            }

            res.groupBy(_.spool).foreach(group => {
              if (group._2.exists(_.fcon3 == "FWT0")){
                group._2.filter(_.compType == "BOLT").foreach(x => x.length = x.length / 2d)
                group._2.filter(_.compType == "NUT").foreach(x => x.length = x.length / 2d)
              }
            })


            val sups = ListBuffer.empty[PipeSup]
            DBManager.GetOracleConnection(project) match {
              case Some(conn) =>
                val stmt = conn.createStatement()
                val query = s"SELECT \n    (SELECT STOCK_CODE FROM AS_SUBAS WHERE AS_OID = VSUPP.OID) AS STOCK_CODE,\n    USERID\nFROM \n    (SELECT OID, USERID FROM V_SUPP_LIST WHERE SYSTEM = '$system') VSUPP"
                val rs = stmt.executeQuery(query)
                while (rs.next()){
                  sups += PipeSup(
                    Option(rs.getString("STOCK_CODE")).getOrElse(""),
                    Option(rs.getString("USERID")).getOrElse("")
                  )
                }
                rs.close()
                stmt.close()
                conn.close()
              case _ => None
            }

            var spool = 700
            sups.groupBy(x => x.code).foreach(gr => {
              materials.find(_.code == gr._1) match {
                case Some(material) =>
                  res += PipeSeg(project, "", system, "", 0, 0, "SUP", "Support", "", "SUP", gr._2.map(_.userId).mkString(","), "", 0, 0, 0, "", "SUP", gr._2.length, 0, 0, material.singleWeight, gr._1, "", "", "", material, system)
                  spool += 1
                case _ => None
              }
            })

            var counter = 0
            res.filter(_.typeCode == "SUP").foreach(s => {
              counter += 1
              s.spPieceId = counter
            })

            val devicesAuxFromSystem = ListBuffer.empty[DeviceAux]
            DBManager.GetOracleConnection(project) match {
              case Some(oracle) =>
                val s = oracle.createStatement()
                val query = s"select system, long_descr from systems_lang where system in (select oid from systems where name = '$system')"
                val rs = s.executeQuery(query)
                while (rs.next()) {
                  devicesAuxFromSystem += DeviceAux(
                    Option(rs.getInt("SYSTEM")).getOrElse(-1),
                    Option(rs.getString("LONG_DESCR")).getOrElse(""),
                  )
                }
                rs.close()
                s.close()
                oracle.close()
              case _ => List.empty[Device]
            }
            devicesAuxFromSystem.filter(_.descr.contains("|")).foreach(d => {
              d.descr.split('\n').toList.foreach(l => {
                val split = l.split('|')
                val pos = split.head
                if (split.length >= 4){
                  res += PipeSeg(
                    project, "", system, "", 0, 0, "AUX", "", "",
                    "AUX", "Inserted Manually", "", 0, 0, 0, pos, pos, split(3).toDoubleOption.getOrElse(0),
                    0, 0, materials.find(_.code == split(1)) match {
                      case Some(value) => value.singleWeight
                      case _ => 0
                    }, split(1), "", "", "", materials.find(_.code == split(1)) match {
                      case Some(value) =>
                        if (split.length > 4){
                          value.copy(name = value.name + ", " + split(4), translations = value.translations.map(x => x.copy(name = x.name + ", " + split(4))))
                        }
                        else{
                          value
                        }
                      case _ => Material()
                    }
                  )
                }
              })
            })
            var spoolValue = ""
            var spCounter = 0
            res.filter(_.compType == "AUX").filter(_.spPieceId == 0).sortBy(_.spool).foreach(sp => {
              if (sp.spool != spoolValue){
                spCounter = 0
                val alreadyIn = res.filter(_.compType != "AUX").filter(_.spool == sp.spool)
                if (alreadyIn.nonEmpty){
                  spCounter = alreadyIn.maxBy(_.spPieceId).spPieceId
                }
                spoolValue = sp.spool
              }
              spCounter = spCounter + 1
              sp.spPieceId = spCounter
            })
            res.toList
          case _ => List.empty[PipeSeg]
        }
    }
    pipeSegs
  }
  def getPipeSegsBySystem(project: String, system: String): List[PipeSegExtended] ={
    val pipeSegs = DBManager.GetMongoCacheConnection() match {
      case Some(mongo) =>
        val vPipeCompCollectionActualName = "vPipeCompActualExt"
        val vPipeCompActualCollection: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeCompCollectionActualName)
        DBManager.GetMongoConnection() match {
          case Some(mongoData) =>
            val materialsNCollectionName = "materials-n"
            val projectNamesCollection: MongoCollection[ProjectName] = mongoData.getCollection("project-names")
            val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[ProjectName] => values.toList
              case _ => List.empty[ProjectName]
            }
            val rkdProject = projectNames.find(_.foran == project) match {
              case Some(value) => value.rkd
              case _ => ""
            }
            val materials = getMaterials.filter(_.project == rkdProject)
            val systemDefs = getSystemDefs(project)
            val res = ListBuffer.empty[PipeSegExtended]
            Await.result(vPipeCompActualCollection.find().toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[PipeSegActual] =>
                Await.result(mongo.getCollection[PipeSegExtended](values.last.name).find(and(equal("project", project), equal("system", system))).toFuture(), Duration(300, SECONDS)) match {
                  case pipeSegs: Seq[PipeSegExtended] =>
                    pipeSegs.foreach(x => x.material = materials.find(_.code == x.stock) match {
                      case Some(value) => value
                      case _ => Material()
                    })
                    pipeSegs.foreach(p => p.systemDescr = systemDefs.find(_.name == p.system) match {
                      case Some(systemDef) => systemDef.descr
                      case _ => ""
                    })
                    res ++= pipeSegs.filter(_.typeCode != "GASK")
                  case _ =>
                }
              case _ =>
            }
            res.toList
          case _ => List.empty[PipeSegExtended]
        }
    }
    pipeSegs
  }

  def getPipeSegsFromMongo(project: String, docNumber: String): List[PipeSeg] ={

    val configOracle = new HikariConfig()
    configOracle.setDriverClassName("oracle.jdbc.driver.OracleDriver")
    configOracle.setJdbcUrl("jdbc:oracle:thin:@192.168.1.12:1521:ORA3DB")
    configOracle.setUsername("C" + project)
    configOracle.setPassword("Whatab0utus")
    configOracle.setMaximumPoolSize(5)
    val ds = new HikariDataSource(configOracle)
    val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.36")
    val projectSystem: (String, String) = getSystemAndProjectFromDocNumber(docNumber, mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry), ds.getConnection)
    val mongo = mongoClient.getDatabase("cache").withCodecRegistry(codecRegistry)
    val mongoData = mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    val system = projectSystem._2


    val vPipeCompCollectionActualName = "vPipeCompActual"
    val vPipeCompActualCollection: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeCompCollectionActualName)

    val vPipeJointsCollectionActualName = "vPipeJointsActual"
    val vPipeJointsActualCollection: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeJointsCollectionActualName)

    val materialsNCollectionName = "materials-n"
    val materialsCollection: MongoCollection[Material] = mongoData.getCollection(materialsNCollectionName)
    val projectNamesCollection: MongoCollection[ProjectName] = mongoData.getCollection("project-names")

    val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
      case values: Seq[ProjectName] => values.toList
      case _ => List.empty[ProjectName]
    }

    val rkdProject = projectNames.find(_.foran == project) match {
      case Some(value) => value.rkd
      case _ => ""
    }
    val materials = Await.result(materialsCollection.find(equal("project", rkdProject)).toFuture(), Duration(30, SECONDS)) match {
      case values: Seq[Material] => values.toList
      case _ => List.empty[Material]
    }

    val systemDefs = getSystemDefs(project)
    val res = ListBuffer.empty[PipeSeg]
    Await.result(vPipeCompActualCollection.find().toFuture(), Duration(30, SECONDS)) match {
      case values: Seq[PipeSegActual] =>
        Await.result(mongo.getCollection[PipeSeg](values.last.name).find(and(equal("project", project), if (system != "") equal("system", system) else notEqual("system", system))).toFuture(), Duration(300, SECONDS)) match {
          case pipeSegs: Seq[PipeSeg] =>
            pipeSegs.foreach(x => x.material = materials.find(_.code == x.stock) match {
              case Some(value) => value
              case _ => Material()
            })
            pipeSegs.foreach(p => p.systemDescr = systemDefs.find(_.name == p.system) match {
              case Some(systemDef) => systemDef.descr
              case _ => ""
            })
            res ++= pipeSegs.filter(_.typeCode != "GASK")
          case _ =>
        }
      case _ =>
    }
    Await.result(vPipeJointsActualCollection.find().toFuture(), Duration(30, SECONDS)) match {
      case values: Seq[PipeSegActual] =>
        Await.result(mongo.getCollection[PipeSeg](values.last.name).find(and(equal("project", project), if (system != "") equal("system", system) else notEqual("system", system))).toFuture(), Duration(300, SECONDS)) match {
          case pipeSegs: Seq[PipeSeg] =>
            pipeSegs.foreach(x => x.material = materials.find(_.code == x.stock) match {
              case Some(value) => value
              case _ => Material()
            })
            pipeSegs.foreach(p => p.systemDescr = systemDefs.find(_.name == p.system) match {
              case Some(systemDef) => systemDef.descr
              case _ => ""
            })
            res ++= pipeSegs.filter(_.stock != "")
          case _ =>
        }
      case _ =>
    }

    res.groupBy(_.spool).foreach(group => {
      if (group._2.exists(_.fcon3 == "FWT0")){
        group._2.filter(_.compType == "BOLT").foreach(x => x.length = x.length / 2d)
        group._2.filter(_.compType == "NUT").foreach(x => x.length = x.length / 2d)
      }
    })


    val sups = ListBuffer.empty[String]
    DBManager.GetOracleConnection(project) match {
      case Some(conn) =>
        val stmt = conn.createStatement()
        val query = s"SELECT STOCK_CODE FROM AS_SUBAS WHERE AS_OID IN (SELECT OID FROM V_SUPP_LIST WHERE SYSTEM = '$system')"
        val rs = stmt.executeQuery(query)
        while (rs.next()){
          sups += Option(rs.getString("STOCK_CODE")).getOrElse("")
        }
        rs.close()
        stmt.close()
        conn.close()
      case _ => None
    }

    var spool = 700
    sups.groupBy(x => x).foreach(gr => {
      materials.find(_.code == gr._1) match {
        case Some(material) =>
          res += PipeSeg(project, "", system, "", 0, 0, "SUP", "Support", "", "SUP", "", "", 0, 0, 0, "", spool.toString, 0, 0, 0, material.singleWeight, gr._1, "", "", "", material, system)
          spool += 1
        case _ => None
      }
    })

    res.toList
  }

  def getPipeSegsBilling(project: String): List[PipeSegBilling] ={
    GetMongoCacheConnection() match {
      case Some(mongo) =>

        val vPipeCompCollectionActualName = "vPipeCompActual"
        val vPipeCompActualCollection: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeCompCollectionActualName)

        GetMongoConnection() match {
          case Some(mongoData) =>
            val materialsNCollectionName = "materials-n"
            val materialsCollection: MongoCollection[Material] = mongoData.getCollection(materialsNCollectionName)
            val projectNamesCollection: MongoCollection[ProjectName] = mongoData.getCollection("project-names")
            val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[ProjectName] => values.toList
              case _ => List.empty[ProjectName]
            }
            val rkdProject = projectNames.find(_.foran == project) match {
              case Some(value) => value.rkd
              case _ => ""
            }
            val materials = Await.result(materialsCollection.find(equal("project", rkdProject)).toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[Material] => values.toList
              case _ => List.empty[Material]
            }
            val systemDefs = getSystemDefs(project)
            val pipeSegs = Await.result(vPipeCompActualCollection.find().toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[PipeSegActual] =>
                Await.result(mongo.getCollection[PipeSeg](values.last.name).find(equal("project", project)).toFuture(), Duration(300, SECONDS)) match {
                  case res: Seq[PipeSeg] => res.foreach(x => x.material = materials.find(_.code == x.stock) match {
                    case Some(value) => value
                    case _ => Material()
                  })
                    res.foreach(p => p.systemDescr = systemDefs.find(_.name == p.system) match {
                      case Some(systemDef) => systemDef.descr
                      case _ => ""
                    })
                    res.toList
                  case _ => List.empty[PipeSeg]
                }
              case _ => List.empty[PipeSeg]
            }

            val result = pipeSegs.groupBy(x => (x.stock, x.insul, x.typeCode, x.compType, x.material)).map(group => {
              val t = group._2.head
              PipeSegBilling(group._2.map(_.zone).distinct.mkString(","), group._2.map(_.system).distinct.mkString(","), t.typeCode, t.typeDesc, t.classAlpha, t.compType, t.compUserId, t.smat,
                group._2.map(_.length).sum, group._2.map(_.weight).sum, t.stock, t.insul, t.material, t.systemDescr, group._2.length)
            }).toList

            result

          case _ => List.empty[PipeSegBilling]
        }

    }
  }
  def getSpoolLocks(docNumber: String): List[SpoolLock] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val spoolLocks: MongoCollection[SpoolLock] = mongo.getCollection("spoolLocks")
        Await.result(spoolLocks.find(equal("docNumber", docNumber)).toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[SpoolLock] =>
            values.toList
          case _ => List.empty[SpoolLock]
        }
      case _ => List.empty[SpoolLock]
    }
  }
  def setSpoolLock(json: String): Unit ={
    decode[SpoolLock](json) match {
      case Right(value) =>
        DBManager.GetMongoConnection() match {
          case Some(mongo) =>
            val locks: MongoCollection[SpoolLock] = mongo.getCollection("spoolLocks")
            value.lock = if (value.lock == 1) 0 else 1
            value.date = new Date().getTime
            Await.result(locks.replaceOne(and(equal("docNumber", value.docNumber), equal("spool", value.spool)), value, org.mongodb.scala.model.ReplaceOptions().upsert(true)).toFuture(), Duration(30, SECONDS))
          case _ =>
        }
      case Left(value) =>
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
          systemDefs += SystemDef(project, Option(rs.getString("NAME")).getOrElse(""),
            Option(rs.getString("DESCR")).getOrElse(""))
        }
        rs.close()
        stmt.close()
        oracleConnection.close()
      case _ =>
    }
    systemDefs.toList
  }
  def getSystemAndProjectFromDocNumber(docNumber: String): (String, String) = {
    DBManager.GetMongoNewConnection() match {
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
                val systems = getSystemDefs(project.foran)
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
  def getSystemAndProjectFromDocNumber(docNumber: String, mongoData: MongoDatabase, oracleConnection: Connection): (String, String) = {
    val projectNamesCollection: MongoCollection[ProjectName] = mongoData.getCollection("project-names")
    val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
      case values: Seq[ProjectName] => values.toList
      case _ => List.empty[ProjectName]
    }
    """\d{6}(?=-\d{3}\w{0,1}-\d{3,4})""".r.findFirstIn(docNumber) match {
      case Some(rkdProject) =>
        projectNames.find(_.rkd == rkdProject) match {
          case Some(project) =>
            val systems = getSystemDefsFromOracle(project.foran, oracleConnection)
            systems.find(x => x.descr.contains(docNumber)) match {
              case Some(system) =>
                (project.foran, system.name)
              case _ => ("", "")
            }
          case _ => ("", "")
        }
      case _ => ("", "")
    }
  }
  def getSystemDefsFromOracle(project: String, oracleConnection: Connection): List[SystemDef] ={
    val systemDefs = ListBuffer.empty[SystemDef]
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
    systemDefs.toList
  }
  def getSpoolModel(docNumber: String, spool: String, isom: Int = 0): File ={
    val res = ListBuffer.empty[String]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select file_url from revision_files where removed = 0 and issue_id in (select id from issue where doc_number = '${docNumber}' and issue_type = 'RKD') and group_name = 'Spool Models'")
        while (rs.next()){
          res += rs.getString("file_url")
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res.find(_.contains("obj")) match {
      case Some(zipUrl) =>
        val spCloud = "/"
        val name = zipUrl.split(spCloud).last
        val enc = zipUrl.replace(zipUrl.split(spCloud).takeRight(1).head, "") + URLEncoder.encode(zipUrl.split(spCloud).takeRight(1).head, "UTF-8")
        val file = File.createTempFile("obj", ".zip")
        val directory = Files.createTempDirectory("obj")
        val url = new URL(enc)
        val stream = url.openStream()
        Files.copy(stream, file.toPath, StandardCopyOption.REPLACE_EXISTING)
        val zipFile = new ZipFile(file)
        for (entry <- zipFile.entries.asScala) {
          if (!entry.isDirectory) {
            Files.copy(zipFile.getInputStream(entry), new File(directory + "/" + entry.getName).toPath, StandardCopyOption.REPLACE_EXISTING)
          }
        }

        val files = directory.toFile.listFiles().toList.filter(_.isFile)

        val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
        val pipeSegs = getPipeSegs(projectSystem._1, projectSystem._2)
        val spoolSegs = pipeSegs.filter(x => (if (isom == 0) x.spool == spool else x.isom == spool) || spool == "full").filter(_.sqInSystem != 0)

        val spoolFiles = files.filter(x => spoolSegs.exists(y => x.getName.contains("-" + y.sqInSystem.toString)))


        if (spoolFiles.nonEmpty){
          val zipOut = File.createTempFile("obj", ".zip")
          val zip = new ZipOutputStream(new FileOutputStream(zipOut))
          spoolFiles.foreach(sFile => {
            zip.putNextEntry(new ZipEntry(sFile.getName))
            zip.write(new FileInputStream(sFile).readAllBytes())
            zip.closeEntry()
          })
          zip.close()
          zipOut
        }
        else{
          val error = File.createTempFile("error", ".txt")
          new FileOutputStream(error).write("error, no spool files".getBytes())
          error
        }

      case _ =>
        val error = File.createTempFile("error", ".txt")
        new FileOutputStream(error).write("error, no model files for document".getBytes())
        error
    }
  }
  def getHvacSegs(project: String, system: String, sqInSystem: Int = -1): List[PipeSeg] ={
    val res = ListBuffer.empty[PipeSeg]
    val projects = getProjects
    val rkdProject = projects.find(_.foran == project) match {
      case Some(value) => value.rkd
      case _ => project
    }
    val materials = getMaterials.filter(_.project == rkdProject)
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val q = Source.fromResource("queries/plsElem.sql").mkString.replace("&system", system) + s" AND (PLS.IDSQ = $sqInSystem OR $sqInSystem = -1)"
        val rsIt1 = stmt.executeQuery(q)
        val plsElems = RsIterator(rsIt1).map(rs => {
          PlsElem(
            Pls(Option(rs.getInt("TYPE")).getOrElse(0),
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
              Option(rs.getString("CMPNAME")).getOrElse(""),
          )
        }).toList

        val rsIt2 = stmt.executeQuery(s"SELECT * FROM PIPELINE_SEGMENT WHERE SYSTEM IN (SELECT SEQID FROM SYSTEMS WHERE NAME LIKE '$system')")
        val pipeLineSegments = RsIterator(rsIt2).map(rs => {
          PipeLineSegment(
            Pls(Option(rs.getInt("TYPE")).getOrElse(0),
              Option(rs.getInt("ZONE")).getOrElse(0),
              Option(rs.getInt("SYSTEM")).getOrElse(0),
              Option(rs.getString("LINE")).getOrElse(""),
              Option(rs.getInt("SQID")).getOrElse(0),
              0),
            Option(rs.getString("BDATRI")).getOrElse(""),
            Option(rs.getInt("OID")).getOrElse(0),
          )
        }).toList

        val rsIt3 = stmt.executeQuery(s"SELECT * FROM PLSE_PAROBJ_REALPAR WHERE SYSTEM IN (SELECT SEQID FROM SYSTEMS WHERE NAME LIKE '$system')")
        val plsParams = RsIterator(rsIt3).map(rs => {
          PlsParam(
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
        }).toList

        val rsIt4 = stmt.executeQuery(s"SELECT * FROM MATERIAL_QUALITY")
        val materialQuality = RsIterator(rsIt4).map(rs => {
          MaterialQuality(
            Option(rs.getString("CODE")).getOrElse(""),
            Option(rs.getString("DESCR")).getOrElse(""),
            Option(rs.getDouble("WEIGHT")).getOrElse(0),
            Option(rs.getDouble("THICKNESS")).getOrElse(0),
          )
        }).toList

        plsElems.foreach(plsElem => {
          val bdatri = pipeLineSegments.find(_.pls.equals(plsElem.pls)) match {
            case Some(value) => value.bdatri
            case _ => ""
          }
          val params = plsParams.filter(_.pls.equals(plsElem.pls)).sortBy(_.paramSq)
          val quality = (bdatri + "    ").substring(0, 4).trim
          val insulation = (bdatri + "    ").substring(4, 4).trim

          if (plsElem.spool == "550"){
            val qwe = 0
          }
          val isComp = plsElem.cmp_oid != 0

          if (isComp){
            materials.find(_.code == plsElem.cmp_stock) match {
              case Some(material) =>
                val l = if (params.nonEmpty){
                  params.last.value / 1000d
                }
                else{
                  0
                }
                res += PipeSeg(
                  project,
                  plsElem.zone,
                  system,
                  plsElem.pls.line,
                  plsElem.pls.pls,
                  plsElem.pls.elem,
                  "HVAC",
                  "COMPONENT",
                  "",
                  plsElem.cType,
                  plsElem.cmpName,
                  quality,
                  plsElem.idsq,
                  plsElem.isPieceId,
                  plsElem.spPieceId,
                  plsElem.isom,
                  plsElem.spool,
                  l,
                  0,
                  0,
                  plsElem.weight,
                  material.code,
                  "",
                  insulation,
                  "",
                  material
                )
              case _ =>
            }
          }
          else{
            materialQuality.find(_.code == quality) match {
              case Some(qa) =>
                """\w{12}\d{4}""".r.findFirstIn(qa.descr) match {
                  case Some(code) =>
                    materials.find(_.code == code) match {
                      case Some(material) =>
                        val l = if (params.nonEmpty){
                          params.last.value / 1000d
                        }
                        else{
                          0
                        }
                        val hvacName = plsElem.cType match {
                          case "B" =>
                            if (params.length == 3){
                              val radius = Math.round(params(0).value * 2)
                              val diam = Math.round(params(1).value)
                              val angle = Math.round(180 / Math.PI * params(2).value)
                              s"ОТВОД $angle°, ДУ$radius (${material.name})"
                            }
                            else if (params.length == 4) {
                              val d1 = Math.round(params(0).value)
                              val d2 = Math.round(params(1).value)
                              val angle = Math.round(180 / Math.PI * params(3).value)
                              s"ОТВОД $angle°, ${d1}x$d2 (${material.name})"
                            }
                            else{
                              "undefined"
                            }
                          case "A" =>
                            if (params.length == 2) {
                              val d1 = Math.round(params(0).value * 2)
                              val d2 = Math.round(params(1).value * 2)
                              s"ПЕРЕХОД ДУ$d2/ДУ$d1 (${material.name})"
                            }
                            else if (params.length == 3){
                              val d1 = Math.round(params(0).value * 2)
                              val d2 = Math.round(params(1).value * 2)
                              val l = Math.round(params(2).value)
                              s"ПЕРЕХОД ДУ$d2/ДУ$d1, L=$l (${material.name})"
                            }
                            else if (params.length == 7) {
                              val d1 = Math.round(params(0).value)
                              val d2 = Math.round(params(1).value)
                              val d3 = Math.round(params(2).value)
                              val d4 = Math.round(params(3).value)
                              val l = Math.round(params.last.value)
                              s"ПЕРЕХОД ${d1}x$d2/${d3}x$d4, L=$l (${material.name})"
                            }
                            else if (params.length == 8) {
                              val d1 = Math.round(params(0).value)
                              val d2 = Math.round(params(1).value)
                              val diam = Math.round(params(2).value)
                              val l = Math.round(params(3).value)
                              s"ПЕРЕХОД ${d1}x$d2/ДУ$diam, L=$l (${material.name})"
                            }
                            else if (params.length == 9) {
                              val d1 = Math.round(params(0).value)
                              val d2 = Math.round(params(1).value)
                              val r = Math.round(params(2).value)
                              val angle = Math.round(180 / Math.PI * params(3).value)
                              val d3 = Math.round(params(4).value)
                              val d4 = Math.round(params(5).value)
                              s"ОТВОД $angle° R=$r С ПЕРЕХОДОМ ${d1}x$d2/${d3}x$d4 (${material.name})"
                            }
                            else if (params.length > 10) {
                              val d1 = Math.round(params(0).value)
                              val d2 = Math.round(params(1).value)
                              val diam = Math.round(params(2).value * 2)
                              val l = Math.round(params(3).value)
                              s"ПЕРЕХОД ${d2}x$d1/ДУ$diam, L=$l (${material.name})"
                            }
                            else{
                              "undefined"
                            }
                          case "P" =>
                            if (params.length == 2){
                              val d = Math.round(params(0).value * 2)
                              val l = Math.round(params(1).value)
                              s"ВОЗДУХОВОД ДУ$d, L=$l (${material.name})"
                            }
                            else if (params.length == 3){
                              val d1 = Math.round(params(0).value)
                              val d2 = Math.round(params(1).value)
                              val l = Math.round(params(2).value)
                              s"ВОЗДУХОВОД ${d2}х${d1}, L=$l (${material.name})"
                            }
                            else{
                              "undefined"
                            }
                          case _ => ""
                        }
                        if (hvacName == "undefined"){
                          val qwe = 0
                        }
                        if (plsElem.spool == "040"){
                          val q = 0
                        }

                        res += PipeSeg(
                          project,
                          plsElem.zone,
                          system,
                          plsElem.pls.line,
                          plsElem.pls.pls,
                          plsElem.pls.elem,
                          "HVAC",
                          "PLATE",
                          "",
                          plsElem.cType,
                          plsElem.cmpName,
                          quality,
                          plsElem.idsq,
                          plsElem.isPieceId,
                          plsElem.spPieceId,
                          plsElem.isom,
                          plsElem.spool,
                          l,
                          0,
                          0,
                          plsElem.weight,
                          material.code,
                          "",
                          insulation,
                          "",
                          material.copy(name = hvacName)
                        )
                      case _ =>
                    }
                  case _ =>
                }
              case _ =>
            }
          }
        })
        rsIt1.close()
        rsIt2.close()
        rsIt3.close()
        rsIt4.close()
        stmt.close()
        connection.close()
      case _ =>
    }
    res.toList
  }

  def getElecEquips(project: String, system: String): List[PipeSeg] = {
    val res = ListBuffer.empty[PipeSeg]
    val projects = getProjects
    val rkdProject = projects.find(_.foran == project) match {
      case Some(value) => value.rkd
      case _ => project
    }
    val materials = getMaterials.filter(_.project == rkdProject)
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val q = Source.fromResource("queries/elecEquips.sql").mkString.replace("&system", system)

        val rsIt = stmt.executeQuery(q)
        val elems = RsIterator(rsIt).map(rs => {
          ElecEquip(
            Option(rs.getInt("COMP")).getOrElse(0),
            Option(rs.getString("USERID")).getOrElse(""),
            Option(rs.getString("ZONE")).getOrElse(""),
            Option(rs.getString("STOCK_CODE")).getOrElse(""),
          )
        }).toList


        elems.foreach(elem => {
          materials.find(_.code == elem.stock) match {
            case Some(material) =>
              res += PipeSeg(
                project,
                elem.zone,
                system,
                "EQUIP",
                0,
                0,
                "EQUIP",
                "COMPONENT",
                "",
                "EQUIP",
                elem.userId,
                "",
                elem.comp,
                1,
                1,
                elem.userId,
                elem.userId,
                0,
                0,
                0,
                material.singleWeight,
                material.code,
                "",
                "NI",
                "",
                material
              )
            case _ =>
          }
        })
        rsIt.close()
        stmt.close()
        connection.close()
      case _ =>
    }
    res.toList
  }
  def getHvacSegs(docNumber: String): List[PipeSeg] ={
    val systemAndProject = getSystemAndProjectFromDocNumber(docNumber)
    val project = systemAndProject._1
    val system = systemAndProject._2
    getHvacSegs(project, system)
  }
}
