package deepsea.esp

import deepsea.database.DBManager
import deepsea.esp.EspManager.EspObject
import org.mongodb.scala.model.Accumulators.addToSet
import org.mongodb.scala.{Document, MongoCollection}
import org.mongodb.scala.model.Aggregates.{group, sort}
import org.mongodb.scala.model.Filters.{and, empty, equal}
import org.mongodb.scala.model.Sorts.descending

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait EspManagerHelper {
  def addEsp(esp: EspObject): Unit ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = "esp-objects"
        val espCollection: MongoCollection[EspObject] = mongo.getCollection(espCollectionName)
        Await.result(espCollection.insertOne(esp).toFuture(), Duration(10, SECONDS))
      case _ => None
    }
  }
  def getLatestEsp(docNumber: String, rev: String): Option[EspObject] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = "esp-objects"
        val espCollection: MongoCollection[EspObject] = mongo.getCollection(espCollectionName)
        Await.result(espCollection.find(and(equal("docNumber", docNumber), equal("rev", rev))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
          case espObject: EspObject => Option(espObject)
          case _ => Option.empty[EspObject]
        }
      case _ => None
    }
  }
  def getAllEsp: List[EspObject] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = "esp-objects"
        val espCollection: MongoCollection[EspObject] = mongo.getCollection(espCollectionName)
        Await.result(espCollection.find().sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
          case espObjects: Seq[EspObject] => espObjects.toList
          case _ => List.empty[EspObject]
        }
      case _ => List.empty[EspObject]
    }
  }
  def getAllLatestEsp: List[EspObject] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = "esp-objects"
        val espCollection = mongo.getCollection(espCollectionName)
        try{
          Await.result(espCollection.aggregate(Seq(sort(descending("date")), group(Document("docNumber" -> "$docNumber"),
            addToSet("id", "$id"),
            addToSet("foranProject", "$foranProject"),
            addToSet("docNumber", "$docNumber"),
            addToSet("rev", "$rev"),
            addToSet("date", "$date"),
            addToSet("elements", "$elements"),
          ))).toFuture(), Duration(10, SECONDS)) match {
            case espObjects: Seq[Document] =>
              val q = espObjects.toList
              val qa = q
              List.empty[EspObject]
            case _ => List.empty[EspObject]
          }
        }
        catch {
          case e: Exception =>
            println(e.toString)
            List.empty[EspObject]
        }
      case _ => List.empty[EspObject]
    }
  }
}
