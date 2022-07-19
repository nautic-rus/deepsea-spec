package deepsea.pipe

import akka.actor.{Actor, ActorSystem}
import akka.http.scaladsl.{Http, HttpExt}
import akka.pattern.ask
import akka.util.Timeout
import deepsea.actors.ActorManager
import deepsea.database.DatabaseManager
import deepsea.database.DatabaseManager.{GetConnection, GetMongoCacheConnection, GetMongoConnection, GetOracleConnection}
import deepsea.files.FileManager.GenerateUrl
import deepsea.pipe.PipeManager.{GetPipeSegs, GetPipeSegsBilling, GetPipeSegsByDocNumber, GetPipeSegsInSystem, GetSpoolLocks, GetSystems, GetZones, Material, PipeSeg, PipeSegActual, PipeSegBilling, ProjectName, SetSpoolLock, SpoolLock, SystemDef, UpdatePipeComp, UpdatePipeJoints}
import org.mongodb.scala.bson.BsonDocument
import org.mongodb.scala.model.Filters.{all, and, equal, in, notEqual}
import org.mongodb.scala.{Document, MongoCollection, bson}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import org.mongodb.scala.model.Filters
import io.circe.parser.decode

import java.util.concurrent.TimeUnit
import java.util.{Date, UUID}
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration.{Duration, DurationInt, SECONDS}


object PipeManager{

  case class PipeSeg(project: String, zone: String, system: String, line: String, pls: Int, elem: Int, typeCode: String, typeDesc: String, classAlpha: String, compType: String, compUserId: String, smat: String, sqInSystem: Int, isPieceId: Int, spPieceId: Int, isom: String, spool: String, length: Double, radius: Double, angle: Double, weight: Double, stock: String, insul: String, var material: Material = Material(), var systemDescr: String = "")
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
  case class SpoolLock(issueId: Int, docNumber: String, spool: String, var lock: Int, user: String, var date: Long)

  case class UpdatePipeComp()
  case class UpdatePipeJoints()
  case class GetPipeSegs(project: String, system: String = "", sqInSystem: String = "")
  case class GetPipeSegsBilling(project: String)
  case class GetPipeSegsSpools(project: String)
  case class GetPipeSegsByDocNumber(docNumber: String, json: Boolean = true)
  case class GetSystems(project: String)
  case class GetZones(project: String)
  case class GetSpoolLocks(docNumber: String)
  case class SetSpoolLock(jsValue: String)





}
class PipeManager extends Actor with Codecs with PipeHelper {

  implicit val system: ActorSystem = ActorSystem()
  implicit val executionContext: ExecutionContextExecutor = system.dispatcher
  val executor: ExecutionContextExecutor = system.dispatcher
  implicit val timeout: Timeout = Timeout(60, TimeUnit.SECONDS)

  override def preStart(): Unit = {
    self ! UpdatePipeJoints()
    system.scheduler.scheduleWithFixedDelay(0.seconds, 3.minutes, self, UpdatePipeComp())
    system.scheduler.scheduleWithFixedDelay(0.seconds, 3.minutes, self, UpdatePipeJoints())
  }

  override def receive: Receive = {

    case UpdatePipeComp() => updatePipeComp()
    case UpdatePipeJoints() => updatePipeJoints()


    case GetSystems(project) => sender() ! getSystems(project).asJson.noSpaces
    case GetZones(project) => sender() ! getZones(project).asJson.noSpaces
    case GetPipeSegs(project, system, sqInSystem) => sender() ! getPipeSegs(project, system, sqInSystem).asJson.noSpaces
    case GetPipeSegsBilling(project) => sender() ! getPipeSegsBilling(project).asJson.noSpaces
    case GetPipeSegsByDocNumber(docNumber, json) =>
      val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
      val pipeSegs = getPipeSegs(projectSystem._1, projectSystem._2)
      sender() ! (if (json) pipeSegs.asJson.noSpaces else pipeSegs)
    case SetSpoolLock(jsValue) =>
      setSpoolLock(jsValue)
      sender() ! "success".asJson.noSpaces
    case GetSpoolLocks(docNumber) =>
      sender() ! getSpoolLocks(docNumber).asJson.noSpaces
    case _ => None

  }

  def updatePipeComp(): Unit = {
    val pipeSegs = ListBuffer.empty[PipeSeg]
    List("N002", "N004").foreach(proj => {
      GetOracleConnection(proj) match {
        case Some(c) =>
          val s = c.createStatement()
          val query = s"select * from v_pipecomp"
          val rs = s.executeQuery(query)
          while (rs.next()) {
            pipeSegs += PipeSeg(project = proj, zone = rs.getString("ZONENAME") match {
              case value: String => value
              case _ => ""
            }, system = rs.getString("SYSTEMNAME") match {
              case value: String => value
              case _ => ""
            },
            line = Option(rs.getString("LINE")).getOrElse(""),
            pls = Option(rs.getInt("PLS")).getOrElse(0),
            elem = Option(rs.getInt("ELEM")).getOrElse(0),
            typeCode = rs.getString("TYPECODE") match {
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
            }, isPieceId = rs.getInt("ISPIECEID") match {
              case value: Int => value
              case _ => 0
            }, spPieceId = rs.getInt("SPPIECEID") match {
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
            if (caches.length > 3) {
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
    List("N002", "N004", "TEST").foreach(proj => {
      GetOracleConnection(proj) match {
        case Some(c) =>
          val s = c.createStatement()
          val query = s"select * from v_pipejoins"
          val rs = s.executeQuery(query)
          while (rs.next()) {
            val zoneName = Option(rs.getString("ZONENAME")).getOrElse("")
            val systemName = Option(rs.getString("SYSTEMNAME")).getOrElse("")
            val gasket = Option(rs.getString("JOINSTOCKCODE")).getOrElse("")
            val bolts = Option(rs.getString("BOLTS1STOCKCODE")).getOrElse("")
            val nuts = Option(rs.getString("NUTS1STOCKCODE")).getOrElse("")
            val boltsNumber = Option(rs.getInt("BOLTS1NUMBER")).getOrElse(0)
            val nutsNumber = Option(rs.getInt("NUTS1NUMBER")).getOrElse(0)
            val jointNumber = Option(rs.getInt("JOINTNUMBER")).getOrElse(0)
            val isomId = Option(rs.getInt("ISOMID")).getOrElse(0)
            val isomUserId = Option(rs.getString("ISOMUSERID")).getOrElse("")
            val spoolUserId = Option(rs.getString("SPOOLUSERID")).getOrElse("")
            val smat = Option(rs.getString("SMAT")).getOrElse("")
            val apClass = Option(rs.getString("APCLASS")).getOrElse("")

            pipeSegs += PipeSeg(proj, zoneName, systemName, "", 0, 0, "JOINT", "", apClass, "GASKET", "", smat, 0, 100, 100, isomUserId, spoolUserId, jointNumber, 0, 0, 0, gasket.trim, "")
            pipeSegs += PipeSeg(proj, zoneName, systemName, "", 0, 0, "JOINT", "", apClass, "BOLT", "", smat, 0, 101, 101, isomUserId, spoolUserId, boltsNumber, 0, 0, 0, bolts.trim, "")
            pipeSegs += PipeSeg(proj, zoneName, systemName, "", 0, 0, "JOINT", "", apClass, "NUT", "", smat, 0, 102, 102, isomUserId, spoolUserId, nutsNumber, 0, 0, 0, nuts.trim, "")

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
            if (caches.length > 3) {
              caches.take(caches.length - 3).foreach(x => Await.result(mongo.getCollection(x).drop().toFuture(), Duration(30, SECONDS)))
            }
          case _ =>
        }

        Await.result(vPipeCompActual.deleteMany(notEqual("date", date)).toFuture(), Duration(30, SECONDS))


      case _ =>
    }
  }
}