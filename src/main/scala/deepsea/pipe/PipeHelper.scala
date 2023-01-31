package deepsea.pipe

import deepsea.database.DatabaseManager._
import deepsea.pipe.PipeManager.{GetPipeSegs, GetPipeSegsBilling, GetPipeSegsByDocNumber, GetSpoolLocks, GetSystems, GetZones, Material, PipeSeg, PipeSegActual, PipeSegBilling, PipeSup, ProjectName, SetSpoolLock, SpoolLock, SystemDef, Units, UpdatePipeComp, UpdatePipeJoints}
import org.mongodb.scala.{Document, MongoClient, MongoCollection, MongoDatabase, bson}
import org.mongodb.scala.model.Filters.{and, equal, notEqual}
import akka.http.scaladsl.{Http, HttpExt}
import akka.pattern.ask
import akka.util.Timeout
import com.zaxxer.hikari.{HikariConfig, HikariDataSource}
import deepsea.actors.ActorManager
import deepsea.database.{DBManager, DatabaseManager}
import deepsea.files.FileManager.GenerateUrl
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.Filters.{all, and, equal, in, notEqual}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import org.mongodb.scala.model.Filters
import io.circe.parser.decode

import java.io.{File, FileInputStream, FileOutputStream}
import java.net.{URL, URLEncoder}
import java.nio.file.{Files, StandardCopyOption}
import java.sql.Connection
import java.util.Date
import java.util.zip.{ZipEntry, ZipFile, ZipOutputStream}
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.jdk.CollectionConverters.EnumerationHasAsScala
import scala.reflect.io.Directory

trait PipeHelper extends Codecs {
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
    GetMongoCacheConnection() match {
      case Some(mongo) =>

        val vPipeCompCollectionActualName = "vPipeCompActual"
        val vPipeCompActualCollection: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeCompCollectionActualName)

        val vPipeJointsCollectionActualName = "vPipeJointsActual"
        val vPipeJointsActualCollection: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeJointsCollectionActualName)

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
                conn.close()
              case _ => None
            }

            var spool = 700
            sups.groupBy(x => x.code).foreach(gr => {
              materials.find(_.code == gr._1) match {
                case Some(material) =>
                  res += PipeSeg(project, "", system, "", 0, 0, "SUP", "Support", "", "SUP", gr._2.map(_.userId).mkString(","), "", 0, 0, 0, "", "S" + spool.toString, gr._2.length, 0, 0, material.singleWeight, gr._1, "", "", material, system)
                  spool += 1
                case _ => None
              }
            })

            res.toList

          case _ => List.empty[PipeSeg]
        }

    }
  }
  def getPipeSegsFromMongo(project: String, docNumber: String): List[PipeSeg] ={

    val configOracle = new HikariConfig()
    configOracle.setDriverClassName("oracle.jdbc.driver.OracleDriver")
    configOracle.setJdbcUrl("jdbc:oracle:thin:@office.nautic-rus.ru:1521:ORA3DB")
    configOracle.setUsername("C" + project)
    configOracle.setPassword("Whatab0utus")
    configOracle.setMaximumPoolSize(5)
    val ds = new HikariDataSource(configOracle)
    val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26")
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
            val jk = pipeSegs
            val jkk = jk
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
        conn.close()
      case _ => None
    }

    var spool = 700
    sups.groupBy(x => x).foreach(gr => {
      materials.find(_.code == gr._1) match {
        case Some(material) =>
          res += PipeSeg(project, "", system, "", 0, 0, "SUP", "Support", "", "SUP", "", "", 0, 0, 0, "", spool.toString, 0, 0, 0, material.singleWeight, gr._1, "", "", material, system)
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
    DatabaseManager.GetMongoConnection() match {
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
        DatabaseManager.GetMongoConnection() match {
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
    GetOracleConnection(project) match {
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
  def getSystemAndProjectFromDocNumber(docNumber: String): (String, String) = {
    GetMongoConnection() match {
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
                  case _ => ("", "")
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
}
