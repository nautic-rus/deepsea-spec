package deepsea.materials

import deepsea.database.DBManager
import deepsea.database.DatabaseManager.GetMongoConnection
import deepsea.pipe.PipeManager.{Material, ProjectName}
import org.mongodb.scala.MongoCollection

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

trait MaterialsHelper {
  def getMaterials: List[Material] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n").find[Material]().toFuture(), Duration(30, SECONDS)) match {
          case dbMaterials => dbMaterials.toList
          case _ => List.empty[Material]
        }
      case _ => List.empty[Material]
    }
  }
}
