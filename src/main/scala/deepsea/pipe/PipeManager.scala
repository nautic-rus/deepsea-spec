package deepsea.pipe

import akka.actor.{Actor, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import deepsea.database.DatabaseManager
import deepsea.database.DatabaseManager.{GetConnection, GetMongoCacheConnection, GetMongoConnection, GetOracleConnection}
import deepsea.pipe.PipeManager.{GetPipeSegs, GetPipeSegsBilling, GetPipeSegsByDocNumber, GetSystems, GetZones, Material, PipeSeg, PipeSegActual, PipeSegBilling, ProjectName, SetSpoolLock, SpoolLock, SystemDef, UpdatePipeComp, UpdatePipeJoints}
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.Filters.{all, and, equal, in, notEqual}
import org.mongodb.scala.{Document, MongoCollection, bson}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import org.mongodb.scala.model.Filters
import io.circe.parser.decode

import java.util.{Date, UUID}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration.{Duration, DurationInt, SECONDS}


object PipeManager{

  case class PipeSeg(project: String, zone: String, system: String, typeCode: String, typeDesc: String, classAlpha: String, compType: String, compUserId: String, smat: String, sqInSystem: Int, isPieceId: Int, spPieceId: Int, isom: String, spool: String, length: Double, radius: Double, angle: Double, weight: Double, stock: String, insul: String, var material: Material = Material(), var systemDescr: String = "")
  case class PipeSegBilling(zone: String, system: String, typeCode: String, typeDesc: String, classAlpha: String, compType: String, compUserId: String, smat: String, length: Double, weight: Double, stock: String, insul: String, material: Material = Material(), systemDescr: String = "", count: Int = 1)
  case class PipeSegActual(name: String, date: Long)
  case class Material(
                       name: String = "",
                       description: String = "",
                       category: String = "",
                       code: String = "",
                       units: String = "",
                       singleWeight: Double = 0,
                       projects: List[String] = List.empty[String],
                       document: String = "",
                       provider: String = "",
                       note: String = "",
                       comment: String = "",
                       coefficient: Double = 1,
                       id: String = UUID.randomUUID().toString)
  case class ProjectName(id: String, rkd: String, pdsp: String, foran: String)
  case class SystemDef(project: String, name: String, descr: String)
  case class SpoolLock(docNumber: String, spool: String, var lock: Int)

  case class UpdatePipeComp()
  case class UpdatePipeJoints()
  case class GetPipeSegs(project: String, system: String)
  case class GetPipeSegsBilling(project: String)
  case class GetPipeSegsByDocNumber(docNumber: String, json: Boolean = true)
  case class GetSystems(project: String)
  case class GetZones(project: String)
  case class GetSpoolLocks(docNumber: String)
  case class SetSpoolLock(jsValue: String)
}
class PipeManager extends Actor with Codecs{

  implicit val system: ActorSystem = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  val executor: ExecutionContextExecutor = system.dispatcher

  override def preStart(): Unit = {
    self ! UpdatePipeJoints()
    system.scheduler.scheduleWithFixedDelay(0.seconds, 15.minutes, self, UpdatePipeComp())
    system.scheduler.scheduleWithFixedDelay(0.seconds, 15.minutes, self, UpdatePipeJoints())
    val qwe = getPipeSegs("TEST", "582-01")
  }

  override def receive: Receive = {

    case UpdatePipeComp() => updatePipeComp()
    case UpdatePipeJoints() => updatePipeJoints()


    case GetSystems(project) => sender() ! getSystems(project).asJson.noSpaces
    case GetZones(project) => sender() ! getZones(project).asJson.noSpaces
    case GetPipeSegs(project, system) => sender() ! getPipeSegs(project, system).asJson.noSpaces
    case GetPipeSegsBilling(project) => sender() ! getPipeSegsBilling(project).asJson.noSpaces
    case GetPipeSegsByDocNumber(docNumber, json) =>
      val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
      val pipeSegs = getPipeSegs(projectSystem._1, projectSystem._2)
      sender() ! (if (json) pipeSegs.asJson.noSpaces else pipeSegs)
    case SetSpoolLock(jsValue) =>
      setSpoolLock(jsValue)
      sender() ! "success".asJson.noSpaces
    case _ => None

  }
  def updatePipeComp(): Unit = {
    val pipeSegs = ListBuffer.empty[PipeSeg]
    List("N002","N004").foreach(proj => {
      GetOracleConnection(proj) match {
        case Some(c) =>
          val s = c.createStatement()
          val query = s"select * from v_pipecomp"
          val rs = s.executeQuery(query)
          while (rs.next()){
            pipeSegs += PipeSeg(project = proj, zone = rs.getString("ZONENAME") match {
              case value: String => value
              case _ => ""
            }, system = rs.getString("SYSTEMNAME") match {
              case value: String => value
              case _ => ""
            }, typeCode = rs.getString("TYPECODE") match {
              case value: String => value
              case _ => ""
            }, typeDesc = rs.getString("TYPEDESC") match {
              case value: String => value
              case _ => ""
            }, classAlpha = rs.getString("CLASSALPHA") match {
              case value: String => value
              case _ => ""
            }, compType = rs.getString("COMPTYPE") match {
              case value: String => value
              case _ => ""
            }, compUserId = rs.getString("COMPUSERID") match {
              case value: String => value
              case _ => ""
            }, smat = rs.getString("SMAT") match {
              case value: String => value
              case _ => ""
            }, sqInSystem = rs.getInt("SQINSYSTEM") match {
              case value: Int => value
              case _ => 0
            }, isPieceId =  rs.getInt("ISPIECEID") match {
              case value: Int => value
              case _ => 0
            }, spPieceId =  rs.getInt("SPPIECEID") match {
              case value: Int => value
              case _ => 0
            }, isom = rs.getString("ISOMUSERID") match {
              case value: String => value
              case _ => ""
            }, spool = rs.getString("SPOOLUSERID") match {
              case value: String => value
              case _ => ""
            }, length = rs.getDouble("LENGTH") match {
              case value: Double => value
              case _ => 0
            }, radius = rs.getDouble("RADIUS") match {
              case value: Double => value
              case _ => 0
            }, angle = rs.getDouble("ANGLE") match {
              case value: Double => value
              case _ => 0
            }, weight = rs.getDouble("WEIGHT") match {
              case value: Double => value
              case _ => 0
            }, stock = rs.getString("STOCKCODE") match {
              case value: String => value
              case _ => ""
            }, insul = rs.getString("INSULUSERID") match {
              case value: String => value
              case _ => ""
            })
          }
          s.close()
          c.close()
        case _ =>
      }
    })
    GetMongoCacheConnection() match {
      case Some(mongo) =>

        val date = new Date().getTime
        val vPipeCompCollection = "vPipeComp-" + date.toString
        val vPipeCompCollectionActual = "vPipeCompActual"

        val vPipeComp: MongoCollection[PipeSeg] = mongo.getCollection(vPipeCompCollection)
        val vPipeCompActual: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeCompCollectionActual)

        Await.result(vPipeComp.insertMany(pipeSegs.toList).toFuture(), Duration(300, SECONDS))
        Await.result(vPipeCompActual.insertOne(PipeSegActual(vPipeCompCollection, date)).toFuture(), Duration(30, SECONDS))

        Await.result(mongo.listCollectionNames().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[String] =>
            val caches = values.filter(x => x.contains("vPipeComp-") && !x.contains("actual") && x != vPipeCompCollection).sortBy(x => x)
            if (caches.length > 3){
              caches.take(caches.length - 3).foreach(x => Await.result(mongo.getCollection(x).drop().toFuture(), Duration(30, SECONDS)))
            }
          case _ =>
        }

        Await.result(vPipeCompActual.deleteMany(notEqual("date", date)).toFuture(), Duration(30, SECONDS))


      case _ =>
    }
  }
  def updatePipeJoints(): Unit = {
    val pipeSegs = ListBuffer.empty[PipeSeg]
    List("N002","N004", "TEST").foreach(proj => {
      GetOracleConnection(proj) match {
        case Some(c) =>
          val s = c.createStatement()
          val query = s"select * from v_pipejoins"
          val rs = s.executeQuery(query)
          while (rs.next()){
            val zoneName = Option(rs.getString("ZONENAME")).getOrElse("")
            val systemName = Option(rs.getString("SYSTEMNAME")).getOrElse("")
            val gasket = Option(rs.getString("JOINSTOCKCODE")).getOrElse("")
            val bolts = Option(rs.getString("BOLTS1STOCKCODE")).getOrElse("")
            val nuts = Option(rs.getString("NUTS1STOCKCODE")).getOrElse("")
            val boltsNumber = Option(rs.getInt("BOLTS1NUMBER")).getOrElse(0)
            val nutsNumber = Option(rs.getInt("NUTS1NUMBER")).getOrElse(0)
            val jointNumber = Option(rs.getInt("JOINTNUMBER")).getOrElse(0)

            pipeSegs += PipeSeg(proj, zoneName, systemName, "JOINT", "", "", "GASKET", "", "", 0, 0, 0, "", "", jointNumber, 0, 0, 0, gasket.trim, "")
            pipeSegs += PipeSeg(proj, zoneName, systemName, "JOINT", "", "", "BOLT", "", "", 0, 0, 0, "", "", boltsNumber, 0, 0, 0, bolts.trim, "")
            pipeSegs += PipeSeg(proj, zoneName, systemName, "JOINT", "", "", "NUT", "", "", 0, 0, 0, "", "", nutsNumber, 0, 0, 0, nuts.trim, "")

          }
          s.close()
          c.close()
        case _ =>
      }
    })
    GetMongoCacheConnection() match {
      case Some(mongo) =>

        val date = new Date().getTime
        val vPipeCompCollection = "vPipeJoints-" + date.toString
        val vPipeCompCollectionActual = "vPipeJointsActual"

        val vPipeComp: MongoCollection[PipeSeg] = mongo.getCollection(vPipeCompCollection)
        val vPipeCompActual: MongoCollection[PipeSegActual] = mongo.getCollection(vPipeCompCollectionActual)

        Await.result(vPipeComp.insertMany(pipeSegs.toList).toFuture(), Duration(300, SECONDS))
        Await.result(vPipeCompActual.insertOne(PipeSegActual(vPipeCompCollection, date)).toFuture(), Duration(30, SECONDS))

        Await.result(mongo.listCollectionNames().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[String] =>
            val caches = values.filter(x => x.contains("vPipeJoints-") && !x.contains("actual") && x != vPipeCompCollection).sortBy(x => x)
            if (caches.length > 3){
              caches.take(caches.length - 3).foreach(x => Await.result(mongo.getCollection(x).drop().toFuture(), Duration(30, SECONDS)))
            }
          case _ =>
        }

        Await.result(vPipeCompActual.deleteMany(notEqual("date", date)).toFuture(), Duration(30, SECONDS))


      case _ =>
    }
  }

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
  def getPipeSegs(project: String, system: String): List[PipeSeg] ={
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

            val materials = Await.result(materialsCollection.find(equal("projects", rkdProject)).toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[Material] => values.toList
              case _ => List.empty[Material]
            }


            val systemDefs = getSystemDefs(project)
            val res = ListBuffer.empty[PipeSeg]

            Await.result(vPipeCompActualCollection.find().toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[PipeSegActual] =>
                Await.result(mongo.getCollection[PipeSeg](values.last.name).find(and(equal("project", project), equal("system", system))).toFuture(), Duration(300, SECONDS)) match {
                  case pipeSegs: Seq[PipeSeg] =>
                    pipeSegs.foreach(x => x.material = materials.find(_.code == x.stock) match {
                      case Some(value) => value
                      case _ => Material()
                    })
                    pipeSegs.foreach(p => p.systemDescr = systemDefs.find(_.name == p.system) match {
                      case Some(systemDef) => systemDef.descr
                      case _ => ""
                    })
                    res ++= pipeSegs
                  case _ =>
                }
              case _ =>
            }

            Await.result(vPipeJointsActualCollection.find().toFuture(), Duration(30, SECONDS)) match {
              case values: Seq[PipeSegActual] =>
                Await.result(mongo.getCollection[PipeSeg](values.last.name).find(and(equal("project", project), equal("system", system))).toFuture(), Duration(300, SECONDS)) match {
                  case pipeSegs: Seq[PipeSeg] =>
                    pipeSegs.foreach(x => x.material = materials.find(_.code == x.stock) match {
                      case Some(value) => value
                      case _ => Material()
                    })
                    pipeSegs.foreach(p => p.systemDescr = systemDefs.find(_.name == p.system) match {
                      case Some(systemDef) => systemDef.descr
                      case _ => ""
                    })
                    res ++= pipeSegs
                  case _ =>
                }
              case _ =>
            }

            res.toList

          case _ => List.empty[PipeSeg]
        }

    }
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
            val materials = Await.result(materialsCollection.find(equal("projects", rkdProject)).toFuture(), Duration(30, SECONDS)) match {
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
        val userWatches: MongoCollection[SpoolLock] = mongo.getCollection("spoolLocks")
        Await.result(userWatches.find(equal("docNumber", docNumber)).toFuture(), Duration(30, SECONDS)) match {
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
            Await.result(locks.replaceOne(and(equal("docNumber", value.docNumber), equal("docNumber", value.spool)), value, org.mongodb.scala.model.ReplaceOptions().upsert(true)).toFuture(), Duration(30, SECONDS))
          case _ =>
        }
      case Left(value) =>
    }
  }
  private def getSystemDefs(project: String): List[SystemDef] ={
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
  private def getSystemAndProjectFromDocNumber(docNumber: String): (String, String) = {
    GetMongoConnection() match {
      case Some(mongoData) =>
        val projectNamesCollection: MongoCollection[ProjectName] = mongoData.getCollection("project-names")
        val projectNames = Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[ProjectName] => values.toList
          case _ => List.empty[ProjectName]
        }
        """\d{6}(?=-\d{3}-\d{4})""".r.findFirstIn(docNumber) match {
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
}
