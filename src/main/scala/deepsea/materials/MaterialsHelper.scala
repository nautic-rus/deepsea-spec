package deepsea.materials

import deepsea.database.DBManager
import deepsea.database.DatabaseManager.GetMongoConnection
import deepsea.pipe.PipeManager.{Material, ProjectName}
import local.common.DBRequests.MaterialNode
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.equal

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
  def getMaterialNodes(project: String): List[MaterialNode] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n-nodes").find[MaterialNode](equal("project", project)).toFuture(), Duration(60, SECONDS)) match {
          case nodes => nodes.toList
          case _ => List.empty[MaterialNode]
        }
      case _ => List.empty[MaterialNode]
    }
  }
  def getProjects: List[ProjectName] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val projectNamesCollection: MongoCollection[ProjectName] = mongo.getCollection("project-names")
        Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[ProjectName] => values.toList
          case _ => List.empty[ProjectName]
        }
      case _ => List.empty[ProjectName]
    }
  }
}
