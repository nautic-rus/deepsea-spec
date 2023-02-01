package deepsea.pipe

import akka.actor.{Actor, ActorSystem}
import akka.util.Timeout
import deepsea.database.DatabaseManager.{GetMongoCacheConnection, GetOracleConnection}
import deepsea.pipe.PipeManager.{PipeSeg, PipeSegActual, UpdatePipeComp, UpdatePipeJoints}
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.notEqual

import java.util.Date
import java.util.concurrent.TimeUnit
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, ExecutionContextExecutor}
import scala.concurrent.duration.{Duration, DurationInt, SECONDS}

class PipeCache extends Actor{
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
              }, fcon3 = rs.getString("FCON3") match {
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

            pipeSegs += PipeSeg(proj, zoneName, systemName, "", 0, 0, "JOINT", "", apClass, "GASKET", "", smat, 0, 90, 90, isomUserId, spoolUserId, jointNumber, 0, 0, 0, gasket.trim, "", "")
            pipeSegs += PipeSeg(proj, zoneName, systemName, "", 0, 0, "JOINT", "", apClass, "BOLT", "", smat, 0, 91, 91, isomUserId, spoolUserId, boltsNumber, 0, 0, 0, bolts.trim, "", "")
            pipeSegs += PipeSeg(proj, zoneName, systemName, "", 0, 0, "JOINT", "", apClass, "NUT", "", smat, 0, 92, 92, isomUserId, spoolUserId, nutsNumber, 0, 0, 0, nuts.trim, "", "")

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
