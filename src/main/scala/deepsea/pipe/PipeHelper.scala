package deepsea.pipe

import deepsea.database.DatabaseManager
import deepsea.database.DatabaseManager.{GetMongoCacheConnection, GetMongoConnection, GetOracleConnection}
import deepsea.pipe.PipeManager.{Material, PipeSeg, PipeSegActual, PipeSegBilling, ProjectName, SpoolLock, SystemDef}
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.{and, equal, notEqual}
import akka.http.scaladsl.{Http, HttpExt}
import akka.pattern.ask
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.database.DatabaseManager
import deepsea.database.DatabaseManager.{GetConnection, GetMongoCacheConnection, GetMongoConnection, GetOracleConnection}
import deepsea.files.FileManager.GenerateUrl
import deepsea.pipe.PipeManager.{GetPipeSegs, GetPipeSegsBilling, GetPipeSegsByDocNumber, GetSpoolLocks, GetSystems, GetZones, Material, PipeSeg, PipeSegActual, PipeSegBilling, ProjectName, SetSpoolLock, SpoolLock, SystemDef, UpdatePipeComp, UpdatePipeJoints}
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.Filters.{all, and, equal, in, notEqual}
import org.mongodb.scala.{Document, MongoCollection, bson}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import org.mongodb.scala.model.Filters
import io.circe.parser.decode
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

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
  def getPipeSegs(project: String, system: String = "", sqInSystem: String = ""): List[PipeSeg] ={
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
                Await.result(mongo.getCollection[PipeSeg](values.last.name).find(and(equal("project", project), if (system != "") equal("system", system) else notEqual("system", system), if (sqInSystem != "") equal("sqInSystem", sqInSystem) else notEqual("sqInSystem", sqInSystem))).toFuture(), Duration(300, SECONDS)) match {
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
                Await.result(mongo.getCollection[PipeSeg](values.last.name).find(and(equal("project", project), if (system != "") equal("system", system) else notEqual("system", system), if (sqInSystem != "") equal("sqInSystem", sqInSystem) else notEqual("sqInSystem", sqInSystem))).toFuture(), Duration(300, SECONDS)) match {
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
