package local.sql

import deepsea.App
import org.mongodb.scala.{MongoClient, MongoDatabase}

object MongoDB {

  private var mc: Option[MongoClient] = None

  def mongoClient(): MongoClient = {
    mc match {
      case Some(value) => value
      case None => {
        mc = Option[MongoClient](MongoClient(s"mongodb://${App.conf.getString("mongo.host")}:${App.conf.getInt("mongo.port").toString}"))
        mc.get
      }
    }
  }
}
