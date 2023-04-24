package mats

import deepsea.database.DBManager
import deepsea.pipe.PipeManager.Material
import local.common.DBRequests.MaterialNode
import org.mongodb.scala.model.Filters

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object MaterialManagerHelper {
  def getNodes: List[MaterialNode] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n-nodes").find[MaterialNode].toFuture(), Duration(30, SECONDS)) match {
          case nodes => nodes.toList
          case _ => List.empty[MaterialNode]
        }
      case _ => List.empty[MaterialNode]
    }
  }

}
