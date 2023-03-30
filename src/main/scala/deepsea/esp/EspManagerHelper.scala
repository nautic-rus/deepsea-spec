package deepsea.esp

import com.mongodb.client.model.BsonField
import deepsea.database.DBManager
import deepsea.esp.EspManager.{EspElement, EspHistoryObject, EspObject, HullEspObject, PipeEspObject, espKinds, espObjectsCollectionName}
import local.common.Codecs
import local.pdf.ru.common.ReportCommon.Item11Columns
import org.bson.conversions.Bson
import org.mongodb.scala.model.Accumulators.addToSet
import org.mongodb.scala.{Document, MongoCollection, model}
import org.mongodb.scala.model.Aggregates.{addFields, group, project, replaceWith, sort}
import org.mongodb.scala.model.{BsonField, Field}
import org.mongodb.scala.model.Filters.{and, empty, equal}
import org.mongodb.scala.model.Sorts.{ascending, descending}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait EspManagerHelper extends Codecs{
  def addHullEsp(esp: HullEspObject): Unit ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, esp.foranProject, esp.kind).mkString("-").toLowerCase
        val espCollection: MongoCollection[HullEspObject] = mongo.getCollection(espCollectionName)
        Await.result(espCollection.insertOne(esp).toFuture(), Duration(60, SECONDS))
      case _ => None
    }
  }
  def addPipeEsp(esp: PipeEspObject): Unit ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, esp.foranProject, esp.kind).mkString("-").toLowerCase
        val espCollection: MongoCollection[PipeEspObject] = mongo.getCollection(espCollectionName)
        Await.result(espCollection.insertOne(esp).toFuture(), Duration(10, SECONDS))
      case _ => None
    }
  }
  def getHullLatestEsp(foranProject: String, kind: String, docNumber: String, rev: String): Option[HullEspObject] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, foranProject, kind).mkString("-").toLowerCase
        val espCollection: MongoCollection[HullEspObject] = mongo.getCollection(espCollectionName)
        if (rev == ""){
          Await.result(espCollection.find(and(equal("docNumber", docNumber))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
            case espObject: HullEspObject => Option(espObject)
            case _ => Option.empty[HullEspObject]
          }
        }
        else{
          Await.result(espCollection.find(and(equal("docNumber", docNumber), equal("rev", rev))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
            case espObject: HullEspObject => Option(espObject)
            case _ => Option.empty[HullEspObject]
          }
        }
      case _ => None
    }
  }
  def getAllEspHistory(foranProject: String): List[EspHistoryObject] ={
    val res = ListBuffer.empty[EspHistoryObject]
    espKinds.foreach(kind => {
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          val espCollectionName = List(espObjectsCollectionName, foranProject, kind).mkString("-").toLowerCase
          val espCollection: MongoCollection[Document] = mongo.getCollection(espCollectionName)
          Await.result(espCollection.find().toFuture(), Duration(10, SECONDS)) match {
            case espObject: Seq[Document] =>
              res ++= espObject.map(esp => {
                EspHistoryObject(
                  esp.getString("id"),
                  esp.getString("foranProject"),
                  esp.getString("docNumber"),
                  esp.getString("rev"),
                  esp.getLong("date"),
                  esp.getString("user"),
                  esp.getString("kind"),
                  esp.getInteger("taskId"),
                )
              })
            case _ => Option.empty[HullEspObject]
          }
        case _ => None
      }
    })
    res.toList
  }
  def getEspHistory(foranProject: String, docNumber: String): List[EspHistoryObject] ={
    val res = ListBuffer.empty[EspHistoryObject]
    espKinds.foreach(kind => {
      DBManager.GetMongoConnection() match {
        case Some(mongo) =>
          val espCollectionName = List(espObjectsCollectionName, foranProject, kind).mkString("-").toLowerCase
          val espCollection: MongoCollection[Document] = mongo.getCollection(espCollectionName)
          Await.result(espCollection.find(and(equal("docNumber", docNumber))).toFuture(), Duration(10, SECONDS)) match {
            case espObject: Seq[Document] =>
              res ++= espObject.map(esp => {
                EspHistoryObject(
                  esp.getString("id"),
                  esp.getString("foranProject"),
                  esp.getString("docNumber"),
                  esp.getString("rev"),
                  esp.getLong("date"),
                  esp.getString("user"),
                  esp.getString("kind"),
                  esp.getInteger("taskId"),
                )
              })
            case _ => Option.empty[HullEspObject]
          }
        case _ => None
      }
    })
    res.toList
  }
  def getEspHistoryWithElements(foranProject: String, kind: String, docNumber: String): List[EspObject] ={
    val res = ListBuffer.empty[EspObject]
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, foranProject, kind).mkString("-").toLowerCase
        kind match {
          case "hull" =>
            val espCollection: MongoCollection[HullEspObject] = mongo.getCollection(espCollectionName)
            Await.result(espCollection.find(and(equal("docNumber", docNumber))).sort(descending("date")).toFuture(), Duration(10, SECONDS)) match {
              case espObjects: Seq[HullEspObject] =>
                res ++= espObjects
              case _ => None
            }
          case "pipe" =>
            val espCollection: MongoCollection[PipeEspObject] = mongo.getCollection(espCollectionName)
            Await.result(espCollection.find(and(equal("docNumber", docNumber))).sort(descending("date")).toFuture(), Duration(10, SECONDS)) match {
              case espObjects: Seq[PipeEspObject] =>
                res ++= espObjects
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }
    res.toList
  }
  def getPipeLatestEsp(foranProject: String, kind: String, docNumber: String, rev: String): Option[PipeEspObject] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, foranProject, kind).mkString("-").toLowerCase
        val espCollection: MongoCollection[PipeEspObject] = mongo.getCollection(espCollectionName)
        if (rev == ""){
          Await.result(espCollection.find(and(equal("docNumber", docNumber))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
            case espObject: PipeEspObject => Option(espObject)
            case _ => Option.empty[PipeEspObject]
          }
        }
        else{
          Await.result(espCollection.find(and(equal("docNumber", docNumber), equal("rev", rev))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
            case espObject: PipeEspObject => Option(espObject)
            case _ => Option.empty[PipeEspObject]
          }
        }
      case _ => None
    }
  }
  def getAllLatestEsp(projects: List[String] = List("N002", "N004"), kinds: List[String] = espKinds): List[EspObject] ={
    val res = ListBuffer.empty[EspObject]
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        projects.foreach(project => {
          kinds.foreach(kind => {
            val espCollectionName = List(espObjectsCollectionName, project, kind).mkString("-").toLowerCase
            val espCollection: MongoCollection[EspObject] = mongo.getCollection(espCollectionName)
            try{
              Await.result(espCollection.aggregate(
                Seq(
                  sort(ascending("date")),
                  group(
                    Document("_id" -> "$docNumber"),
                    model.BsonField("id", Document("$last" -> "$id")),
                    model.BsonField("foranProject", Document("$last" -> "$foranProject")),
                    model.BsonField("docNumber", Document("$last" -> "$docNumber")),
                    model.BsonField("rev", Document("$last" -> "$rev")),
                    model.BsonField("date", Document("$last" -> "$date")),
                    model.BsonField("user", Document("$last" -> "$user")),
                    model.BsonField("kind", Document("$last" -> "$kind")),
                    model.BsonField("elements", Document("$last" -> "$elements")),
                  )
                )).toFuture(), Duration(10, SECONDS)) match {
                case espObjects: Seq[EspObject] => res ++= espObjects.toList
                case _ => None
              }
            }
            catch {
              case e: Exception => println(e.toString)
            }
          })
        })
      case _ => None
    }
    res.toList
  }
  def generateHullGlobalEsp(projects: List[String]): List[Item11Columns] ={
    val res = ListBuffer.empty[Item11Columns]
    projects.foreach(p => {
      val esps = getAllLatestEsp(List(p), List("hull"))
      val elems = esps.map(_.asInstanceOf[HullEspObject]).flatMap(_.elements)
      elems.groupBy(x => (x.ELEM_TYPE, x.THICKNESS, x.WIDTH, x.MATERIAL)).map(group => {
        val qty = group._2.map(_.QTY).sum
        val weight = group._2.head.WEIGHT_UNIT
        val weightTotal = group._2.map(_.TOTAL_WEIGHT).sum
        res += Item11Columns(
          isHeader = false,
          "",
          group._1._1,
          List(group._1._2, group._1._3.toString).mkString(","),
          group._1._4,
          qty.toString,
          weight.formatted("%.2f"),
          weightTotal.formatted("%.2f")
        )
      })
    })
    res.toList
  }
  def generatePipeGlobalEsp(projects: List[String]): List[Item11Columns] ={
    val res = ListBuffer.empty[Item11Columns]
    projects.foreach(p => {
      val esps = getAllLatestEsp(List(p), List("pipe"))
      val elems = esps.map(_.asInstanceOf[PipeEspObject]).flatMap(_.elements).filter(_.material.code != "")
      elems.groupBy(x => (x.material.code)).map(group => {
//        val qty = group._2.map(_.QTY).sum
//        val weight = group._2.head.WEIGHT_UNIT
//        val weightTotal = group._2.map(_.TOTAL_WEIGHT).sum
//        res += Item11Columns(
//          isHeader = false,
//          "",
//          group._1._1,
//          List(group._1._2, group._1._3.toString).mkString(","),
//          group._1._4,
//          qty.toString,
//          weight.formatted("%.2f"),
//          weightTotal.formatted("%.2f")
//        )
      })
    })
    res.toList
  }

}
