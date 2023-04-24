import deepsea.materials.MaterialsHelper
import local.common.DBRequests.MaterialNode
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, FileInputStream}
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

class FolderFix extends AnyFunSuite with MaterialsHelper {
  val buffTmp = ListBuffer.empty[(Int, String)]
  val excelFile1 = new FileInputStream(new File("c:\\6\\dpmats.xlsx"))
  val workbook1 = new XSSFWorkbook(excelFile1)
  val datatypeSheet1 = workbook1.getSheetAt(0)
  val iterator1 = datatypeSheet1.iterator()
  case class Tmp(a:String,b:String,c:String)
  val items=ListBuffer.empty[Tmp]
  while (iterator1.hasNext) {
    val currentRow = iterator1.next()
    val buff = ListBuffer.empty[String]
    val a0: String = if(currentRow.getCell(0)!=null)currentRow.getCell(0).toString else ""
    val a1: String = if(currentRow.getCell(1)!=null)currentRow.getCell(1).toString else ""
    val a2: String = if(currentRow.getCell(2)!=null)currentRow.getCell(2).toString else ""
    val a3: String = if(currentRow.getCell(3)!=null)currentRow.getCell(3).toString else ""
    val a4: String = if(currentRow.getCell(4)!=null)currentRow.getCell(4).toString else ""
    val a5: String = if(currentRow.getCell(5)!=null)currentRow.getCell(5).toString else ""
    val a6: String = if(currentRow.getCell(6)!=null)currentRow.getCell(6).toString else ""
    val a7: String = if(currentRow.getCell(7)!=null)currentRow.getCell(7).toString else ""
    val a8: String = if(currentRow.getCell(8)!=null)currentRow.getCell(8).toString else ""
    if (a0.nonEmpty) buff += a0
    if (a1.nonEmpty) buff += a1
    if (a2.nonEmpty) buff += a2
    if (a3.nonEmpty) buff += a3
    if (a4.nonEmpty) buff += a4
    if (a5.nonEmpty) buff += a5
    if (a6.nonEmpty) buff += a6
    if (a7.nonEmpty) buff += a7
    if (a8.nonEmpty) buff += a8
    val d = 0
    if(buff.nonEmpty)items+=Tmp(buff(0).toUpperCase,buff(1).toUpperCase,buff(2).toUpperCase)

    //val unitA: String = currentRow.getCell(1).toString
    //buffTmp += Tuple2(name, unitA)
  }

  val pro=getAllFolders()
  val toInsert=ListBuffer.empty[MaterialNode]

  items.foreach(p=>{
    pro.find(s => s.data.equals(p.c)) match {
      case Some(value) => None
      case None => toInsert+=MaterialNode("200101",p.a,p.b,p.c,"Lvov",new Date().getTime)
    }

  })
  val d=0
  insertFolder(toInsert.toList)

  private def insertFolder(mn: List[MaterialNode]): Unit = {
    import org.mongodb.scala.bson.codecs.Macros._
    val mongoDatabase: MongoDatabase = {
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[MaterialNode]), DEFAULT_CODEC_REGISTRY)
      val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }
    val collectionTasks: MongoCollection[MaterialNode] = mongoDatabase.getCollection("materials-n-nodes")
    mn.foreach(node => {
      val res = collectionTasks.insertOne(node).toFuture()
      val updated = Await.result(res, Duration(100, SECONDS))
      println(node.date + " " + node.label_ru)
    })

  }

  private def getAllFolders(): List[MaterialNode] = {
    import org.mongodb.scala.bson.codecs.Macros._
    val mongoDatabase: MongoDatabase = {
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[MaterialNode]), DEFAULT_CODEC_REGISTRY)
      val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }
    val collectionWorkShopMaterial: MongoCollection[MaterialNode] = mongoDatabase.getCollection("materials-n-nodes")
    val allelems = collectionWorkShopMaterial.find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    val list: List[MaterialNode] = allelems.value.get.getOrElse(Seq.empty[MaterialNode]).toList
    list
  }


}
