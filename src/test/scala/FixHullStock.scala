import deepsea.esp.EspManager.HullEspObject
import deepsea.esp.EspManagerHelper
import local.common.DBRequests.MaterialNode
import local.hull.PartManager.{ForanPartsByDrawingNum, PrdPart}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

class FixHullStock  extends AnyFunSuite with EspManagerHelper{
  val allHullDr=List("200101-222-BS12"," 200101-222-BS06"," 200101-222-BS01"," 200101-222-BS10"," 200101-534-005"," 200101-222-104"," 200101-534-004"," 200101-222-BS07"," 200101-222-700"," 200101-222-BS13"," 200101-222-BS02"," 200101-222-103"," 200101-534-003"," 200101-222-101"," 200101-222-BS09"," 200101-222-611"," 200101-222-202"," 200101-222-204"," 200101-222-615"," 200101-222-203"," 200101-222-244"," 200101-222-BS04"," 200101-222-BS03"," 200101-222-BS14"," 200101-222-616"," 200101-222-102"," 200101-222-612"," 200101-534-002"," 200101-222-BS08"," 200101-222-201"," 200101-222-243"," 200101-222-614"," 200101-222-BS11"," 200101-222-BS05"," 200101-534-001"," 200101-222-105"," 200101-222-613"," 200101-228-571"," 200101-222-800")

  val newHull=ListBuffer.empty[HullEspObject]
  allHullDr.foreach(dr=>{
    getHullLatestEsp("N002", "hull", dr.trim, "") match {
      case Some(hullEsp) => {
        val newStocks=ForanPartsByDrawingNum("N002", hullEsp.docNumber)
        val buff=ListBuffer.empty[PrdPart]
        hullEsp.elements.foreach(ppart=>{
          newStocks.find(s => s.PART_CODE == ppart.PART_CODE) match {
            case Some(value) => {
              //value.STOCK_CODE match {
             //   case Some(value) => buff+=ppart.copy(STOCK_CODE = Option(value))
             //   case None => buff+=ppart
             // }
            }
            case None => buff+=ppart
          }
        })
        newHull+=hullEsp.copy(elements =buff.toList )
        println("done "+ hullEsp.docNumber)
      }
      case None => None
    }
    val b=0
  })
  insertHull(newHull.toList)
//esp-objects-n002-hull

  val b=0

  def getAllHull(): List[HullEspObject] = {

    val mongoDatabase: MongoDatabase = {
      import org.mongodb.scala.bson.codecs.Macros._
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[HullEspObject],classOf[PrdPart]), DEFAULT_CODEC_REGISTRY)
      val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }

    val collectionRepl: MongoCollection[HullEspObject] = mongoDatabase.getCollection("esp-objects-n002-hull")
    val allelems = collectionRepl.find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    //allelems.value.get.getOrElse(ListBuffer.empty[WorkShopMaterial].toSeq).toList.filter(p => p.project.equals("P701"))
    allelems.value.get.getOrElse(ListBuffer.empty[HullEspObject].toSeq).toList
  }

  private def insertHull(mn: List[HullEspObject]): Unit = {
    import org.mongodb.scala.bson.codecs.Macros._
    val mongoDatabase: MongoDatabase = {
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[HullEspObject],classOf[PrdPart]), DEFAULT_CODEC_REGISTRY)
      val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }
    val collectionTasks: MongoCollection[HullEspObject] = mongoDatabase.getCollection("esp-objects-n002-hull_STOCK")
    mn.foreach(node => {
      val res = collectionTasks.insertOne(node).toFuture()
      val updated = Await.result(res, Duration(100, SECONDS))
      println(node.date + " " + node.docNumber)
    })
  }



  val h=0
}
