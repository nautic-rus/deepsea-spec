
import cats.instances.duration
import deepsea.esp.EspManager.{EspObject, HullEspObject, PipeEspObject}
import deepsea.materials.MaterialsHelper
import deepsea.pipe.PipeManager
import deepsea.pipe.PipeManager.{Material, MaterialTranslation, PipeSeg}
import local.common.DBRequests.MaterialNode
import local.domain.CommonTypes.{Component, StdPlateMove, StdProfileMove, SystemLang}
import local.hull.PartManager.PrdPart
import local.sql.ConnectionManager
import mats.MaterialManagerHelper
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.model.Filters.{and, equal}
import org.mongodb.scala.{MongoClient, MongoCollection, MongoDatabase}
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}
import java.sql.{ResultSet, Statement}
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.{BufferedSource, Source}
import io.circe.generic.JsonCodec
import io.circe.{Decoder, Encoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.parser.decode
import io.circe.syntax._
import local.hull.BStree.{HullEspObjectEncoder, PipeEspObjectEncoder}
import org.apache.poi.xssf.usermodel.XSSFWorkbook

import scala.collection.mutable


class MaterialsMovement extends AnyFunSuite with MaterialsHelper {
  case class ChangeInstrusctions(before: String, after: String, oid: String = "")

  case class MatFolders(code: String, ruName: String, engName: String)



/*
  val hdr = getAllHull()

  val test2=hdr.filter(d=>d.elements.exists(s=>s.STOCK_CODE=="1"))

  val stockBuff = ListBuffer.empty[String]
  hdr.foreach(dr => {
    dr.elements.foreach(el => stockBuff += el.STOCK_CODE)
  })
  val totals = stockBuff.distinct.sorted.toList
  totals.foreach(s => println(s))
*/


  val h = 0


  val HullMatsOldPlates: List[Material] = {
    val buffTmp = ListBuffer.empty[(Int, String)]
    val excelFile1 = new FileInputStream(new File("c:\\6\\STD_PLATE.xlsx"))
    val workbook1 = new XSSFWorkbook(excelFile1)
    val datatypeSheet1 = workbook1.getSheetAt(0)
    val iterator1 = datatypeSheet1.iterator()
    while (iterator1.hasNext) {
      val currentRow = iterator1.next()
      val name: Int = currentRow.getCell(0).getNumericCellValue.toInt
      val unitA: String = currentRow.getCell(1).toString
      buffTmp += Tuple2(name, unitA)
    }
    val buff = ListBuffer.empty[Material]
    val excelFile = new FileInputStream(new File("c:\\8\\materialsOrig.xlsx"))
    val workbook = new XSSFWorkbook(excelFile)
    val datatypeSheet = workbook.getSheetAt(0)
    val iterator = datatypeSheet.iterator()
    while (iterator.hasNext) {
      val currentRow = iterator.next()
      val name = currentRow.getCell(0).getStringCellValue
      val w = currentRow.getCell(1).getStringCellValue.toDouble
      val unitA = currentRow.getCell(2).getStringCellValue
      val code = currentRow.getCell(3).getStringCellValue
      val itt: Int = buffTmp.toList.find(p => p._2.toUpperCase.equals(code.toUpperCase)) match {
        case Some(value) => value._1
        case None => 0
      }
      buff += Material(name = name, singleWeight = w, units = unitA, code = code, itt = itt)
    }
    buff.filter(s => s.itt != 0).toList
  }
  val HullMatsOldProfiles: List[Material] = {
    val buffTmp = ListBuffer.empty[(Int, String)]
    val excelFile1 = new FileInputStream(new File("c:\\6\\STD_PROFILE_orig.xlsx"))
    val workbook1 = new XSSFWorkbook(excelFile1)
    val datatypeSheet1 = workbook1.getSheetAt(0)
    val iterator1 = datatypeSheet1.iterator()
    while (iterator1.hasNext) {
      val currentRow = iterator1.next()
      val name: Int = currentRow.getCell(0).getNumericCellValue.toInt
      val unitA: String = currentRow.getCell(1).toString.trim
      buffTmp += Tuple2(name, unitA)
    }

    val buff = ListBuffer.empty[Material]
    val excelFile = new FileInputStream(new File("c:\\8\\materialsOrig.xlsx"))
    val workbook = new XSSFWorkbook(excelFile)
    val datatypeSheet = workbook.getSheetAt(0)
    val iterator = datatypeSheet.iterator()
    while (iterator.hasNext) {
      val currentRow = iterator.next()
      val name = currentRow.getCell(0).getStringCellValue
      val w = currentRow.getCell(1).getStringCellValue.toDouble
      val unitA = currentRow.getCell(2).getStringCellValue.trim
      val code = currentRow.getCell(3).getStringCellValue.trim
      if (code == "HU000BS1801100PCB") {
        val jjs = buffTmp.toList.filter(p => p._2.toUpperCase.equals(code.toUpperCase))
        val jj = 0
      }

      val itt: Int = buffTmp.toList.find(p => p._2.toUpperCase.equals(code.toUpperCase)) match {
        case Some(value) => value._1
        case None => 0
      }
      buff += Material(name = name, singleWeight = w, units = unitA, code = code, itt = itt)
    }
    buff.filter(s => s.itt != 0).toList
  }


  //

/*  val newHulss = ListBuffer.empty[HullEspObject]
  hdr.foreach(dr => {
    val buff=ListBuffer.empty[PrdPart]
    dr.elements.foreach(el=>{
      HullMatsOldProfiles.find(s => s.code.trim == el.STOCK_CODE.trim) match {
        case Some(mat) => {
          profi.find(d => d.oid == mat.itt) match {
            case Some(newMat) =>{
              println("CH" + el.STOCK_CODE + " " + newMat.stock)
              buff += el.copy(STOCK_CODE = newMat.stock)
            }
            case None =>  buff+=el
          }
        }
        case None => buff+=el
      }
    })
    dr.elements=buff.toList
    newHulss+=dr
  })*/







  val buff = ListBuffer.empty[Material]
  val profi: List[StdProfileMove] = getStdProfiles()
  val pla: List[StdPlateMove] = getStdPlates()
  pla.foreach(p => {
    HullMatsOldPlates.find(s => s.itt == p.oid) match {
      case Some(value) => buff += value.copy(code = p.stock, itt = 0, project = "200101")
      case None => None
    }
  })
  profi.foreach(p => {
    HullMatsOldProfiles.find(s => s.itt == p.oid) match {
      case Some(value) => buff += value.copy(code = p.stock, itt = 0, project = "200101")
      case None => {
        HullMatsOldPlates.find(d => d.code.toUpperCase.equals(p.stock.toUpperCase)) match {
          case Some(value) => {
            pla.find(a => a.oid == value.itt) match {
              case Some(n) => {
                buff += value.copy(code = n.stock, itt = 0, project = "200101")
                println("NEW " + n.stock + " " + p.oid)
              }
              case None => println(p.stock + " " + p.oid)
            }
          }
          case None => None
        }
      }
    }
  })
  //insertnewMats(buff.toList)
  val t = 0


  val codesToChange: List[ChangeInstrusctions] = {
    val buff = ListBuffer.empty[ChangeInstrusctions]

    val src: BufferedSource = Source.fromFile("c:\\6\\exportV1.csv")("Windows-1251")
    val lines: List[String] = src.getLines.toList
    val buffTMP = ListBuffer.empty[ChangeInstrusctions]
    lines.foreach(line => {
      val nf = line.toString.split(",").toList.filter(s => s.nonEmpty)
      if (nf.length == 2) {
        buffTMP += ChangeInstrusctions(nf(1).toUpperCase, nf(0).toUpperCase)
      }
    })
    src.close()
    buffTMP.groupBy(s => s.after).foreach(gr => {
      buff ++= codeGroupToCode(gr._1, gr._2.toList)
      //println(gr._1 + " " + gr._2.length)
    })
    buff.toList
  }

  val codesToChangePlate: List[ChangeInstrusctions] = {
    val buff = ListBuffer.empty[ChangeInstrusctions]

    val src: BufferedSource = Source.fromFile("c:\\6\\STD_PLATE.csv")("Windows-1251")
    val lines: List[String] = src.getLines.toList
    val buffTMP = ListBuffer.empty[ChangeInstrusctions]
    lines.foreach(line => {
      val nf = line.toString.split(",").toList.filter(s => s.nonEmpty)
      if (nf.length == 3) {
        buffTMP += ChangeInstrusctions(nf(1).toUpperCase, nf(2).toUpperCase, nf(0).toUpperCase)
      } else {
        val f = 0
      }
    })
    src.close()
    buffTMP.groupBy(s => s.after).foreach(gr => {
      buff ++= codeGroupToCode(gr._1, gr._2.toList)
      //println(gr._1 + " " + gr._2.length)
    })
    buff.toList
  }

  val codesToChangePro: List[ChangeInstrusctions] = {
    val buff = ListBuffer.empty[ChangeInstrusctions]

    val src: BufferedSource = Source.fromFile("c:\\6\\PROFILES.csv")("Windows-1251")
    val lines: List[String] = src.getLines.toList
    val buffTMP = ListBuffer.empty[ChangeInstrusctions]
    lines.foreach(line => {
      val nf = line.toString.split(",").toList.filter(s => s.nonEmpty)
      if (nf.length == 3) {
        buffTMP += ChangeInstrusctions(nf(1).toUpperCase, nf(2).toUpperCase, nf(0).toUpperCase)
      } else {
        buffTMP += ChangeInstrusctions("", nf(1).toUpperCase, nf(0).toUpperCase)
        val f = 0
      }
    })
    src.close()
    buffTMP.groupBy(s => s.after).foreach(gr => {
      buff ++= codeGroupToCode(gr._1, gr._2.toList)
      //println(gr._1 + " " + gr._2.length)
    })
    buff.toList
  }

  val mf: List[MatFolders] = {
    val src: BufferedSource = Source.fromFile("c:\\6\\folders.csv ")("Windows-1251")
    val lines: List[String] = src.getLines.toList
    val buff = ListBuffer.empty[MatFolders]
    lines.foreach(line => {
      val nf = line.toString.split(",").toList.filter(s => s.nonEmpty)
      if (nf.length == 3) {
        buff += MatFolders(nf(2).toUpperCase, nf(1).toUpperCase, nf(0).toUpperCase)
      }
    })
    src.close()
    buff.toList
  }

  val materialsNodes: List[MaterialNode] = {
    val ois = new ObjectInputStream(new FileInputStream("c:\\6\\materials-nodes.bin"))
    val m: List[MaterialNode] = ois.readObject.asInstanceOf[List[MaterialNode]]
    ois.close()
    m.filter(p => p.project.equals("200101"))
  }

  val materials: List[PipeManager.Material] = {
    val ois = new ObjectInputStream(new FileInputStream("c:\\6\\materials.bin"))
    val m: List[PipeManager.Material] = ois.readObject.asInstanceOf[List[PipeManager.Material]]
    ois.close()
    m.filter(p => p.project.equals("200101"))
  }

  val components: List[Component] = {
    val ois = new ObjectInputStream(new FileInputStream("c:\\6\\components.bin"))
    val m: List[Component] = ois.readObject.asInstanceOf[List[Component]]
    ois.close()
    m
  }

  val sysLangs: List[SystemLang] = {
    val ois = new ObjectInputStream(new FileInputStream("c:\\6\\syslang.bin"))
    val m: List[SystemLang] = ois.readObject.asInstanceOf[List[SystemLang]]
    ois.close()
    m
  }

  val pl: List[StdPlateMove] = {
    /*    val ois = new ObjectInputStream(new FileInputStream("c:\\6\\pl.bin"))
        val m: List[StdPlateMove] = ois.readObject.asInstanceOf[List[StdPlateMove]]
        ois.close()
        m*/
    //getStdPlates()
    List.empty[StdPlateMove]
  }

  val pro: List[StdProfileMove] = {
    /*    val ois = new ObjectInputStream(new FileInputStream("c:\\6\\pro.bin"))
        val m: List[StdProfileMove] = ois.readObject.asInstanceOf[List[StdProfileMove]]
        ois.close()
        m*/
    getStdProfiles()
    //List.empty[StdProfileMove]
  }

  //val hullDrawings = getAllHull()

  //val h=hullDrawings.find(s=>s.elements.exists(d=>d.STOCK_CODE==null))
  //val pipesDrawings = getAllPipes()
  //val mats: List[Material] =getAllMats()
  //val newMtas: ListBuffer[Material] =ListBuffer.empty[Material]

  //processOracle()
  //val nc=codesToChangePlate++codesToChangePro
  //process(nc)

  //inserNewPipes(pipesDrawings)
  //insertnewMats(newMtas.toList)
  //insertNewHull(hullDrawings)

  //processOracle(codesToChangePlate)
  val f = 0

  /*  val materials: List[PipeManager.Material] =getMaterials
    val oos = new ObjectOutputStream(new FileOutputStream("c:\\6\\materials.bin"))
    oos.writeObject(materials)
    oos.close()*/


  private def getComponents(): List[Component] = {

    ConnectionManager.connectionByProject("N002") match {
      case Some(connection) => {
        val buff = ListBuffer.empty[Component]
        try {
          connection.setAutoCommit(false)
          val buff = ListBuffer.empty[Component]
          val stmt: Statement = connection.createStatement()
          val sql = "select OID, STOCK_CODE from Component where STOCK_CODE is not NULL"
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            buff += Component(Option(rs.getInt("OID")).getOrElse(0), Option(rs.getString("STOCK_CODE")).getOrElse(""))
          }
          stmt.close()
          connection.close()
          buff.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            buff.toList
        }
      }
      case None => ListBuffer.empty[Component].toList
    }

  }

  private def getSystLang(): List[SystemLang] = {
    ConnectionManager.connectionByProject("N002") match {
      case Some(connection) => {
        val buff = ListBuffer.empty[SystemLang]
        try {
          connection.setAutoCommit(false)
          val buff = ListBuffer.empty[SystemLang]
          val stmt: Statement = connection.createStatement()
          val sql = "select system,lang, long_descr from SYSTEMS_LANG where long_descr is not NULL"
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            buff += SystemLang(
              Option(rs.getInt("system")).getOrElse(0),
              Option(rs.getInt("lang")).getOrElse(0),
              Option(rs.getString("long_descr")).getOrElse(""))
          }
          stmt.close()
          connection.close()
          buff.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            buff.toList
        }
      }
      case None => ListBuffer.empty[SystemLang].toList
    }
  }

  private def getStdProfiles(): List[StdProfileMove] = {
    ConnectionManager.connectionByProject("N002") match {
      case Some(connection) => {

        try {
          connection.setAutoCommit(false)
          val buff = ListBuffer.empty[StdProfileMove]
          val stmt: Statement = connection.createStatement()
          val sql = "select oid, STOCK_CODE0 from std_profile where STOCK_CODE0 is not NULL"
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            buff += StdProfileMove(
              Option(rs.getInt("oid")).getOrElse(0),
              Option(rs.getString("STOCK_CODE0")).getOrElse(""))
          }
          stmt.close()
          connection.close()
          buff.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            ListBuffer.empty[StdProfileMove].toList
        }
      }
      case None => ListBuffer.empty[StdProfileMove].toList
    }
  }

  private def getStdPlates(): List[StdPlateMove] = {
    ConnectionManager.connectionByProject("N002") match {
      case Some(connection) => {

        try {
          connection.setAutoCommit(false)
          val buff = ListBuffer.empty[StdPlateMove]
          val stmt: Statement = connection.createStatement()
          val sql = "select oid, STORAGE_CODE from cn002.STD_PLATE where STORAGE_CODE is not NULL"
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            buff += StdPlateMove(
              Option(rs.getInt("oid")).getOrElse(0),
              Option(rs.getString("STORAGE_CODE")).getOrElse(""))
          }
          stmt.close()
          connection.close()
          buff.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            ListBuffer.empty[StdPlateMove].toList
        }
      }
      case None => ListBuffer.empty[StdPlateMove].toList
    }
  }

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

  private def insertnewMats(mn: List[Material]): Unit = {
    import org.mongodb.scala.bson.codecs.Macros._
    val mongoDatabase: MongoDatabase = {
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[Material], classOf[MaterialTranslation]), DEFAULT_CODEC_REGISTRY)
      val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }
    val collectionTasks: MongoCollection[Material] = mongoDatabase.getCollection("materials-n")
    mn.foreach(node => {
      val res = collectionTasks.insertOne(node).toFuture()
      val updated = Await.result(res, Duration(100, SECONDS))
      println("materials " + node.code + " " + node.project)
    })
  }

  private def inserNewPipes(in: List[PipeEspObject]) = {
    import org.mongodb.scala.bson.codecs.Macros._
    val mongoDatabase: MongoDatabase = {
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[PipeEspObject],
        classOf[PipeSeg], classOf[Material], classOf[MaterialTranslation]), DEFAULT_CODEC_REGISTRY)
      val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }
    val collectionWorkShopMaterial: MongoCollection[PipeEspObject] = mongoDatabase.getCollection("esp-objects-n002-pipe1")
    in.foreach(node => {
      val res = collectionWorkShopMaterial.insertOne(node).toFuture()
      val updated = Await.result(res, Duration(100, SECONDS))
      println("pipes " + node.docNumber + " " + node.foranProject)
    })
  }

  private def insertNewHull(in: List[HullEspObject]) = {
    import org.mongodb.scala.bson.codecs.Macros._
    val mongoDatabase: MongoDatabase = {
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[HullEspObject], classOf[PrdPart]), DEFAULT_CODEC_REGISTRY)
      val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }
    val collectionWorkShopMaterial: MongoCollection[HullEspObject] = mongoDatabase.getCollection("esp-objects-n002-hull_STOCK2")
    in.foreach(node => {
      val res = collectionWorkShopMaterial.insertOne(node).toFuture()
      val updated = Await.result(res, Duration(100, SECONDS))
      println("hull " + node.docNumber + " " + node.foranProject)
    })
  }


  def codeGroupToCode(groupName: String, values: List[ChangeInstrusctions]): List[ChangeInstrusctions] = {
    val ret = ListBuffer.empty[ChangeInstrusctions]
    val nn = groupName.length match {
      case 6 => groupName + "XXXXXX"
      case 9 => groupName + "XXX"
      case _ => groupName
    }
    values.indices.foreach(d => {
      val v = values(d)
      val newAfter: String = nn + intToStr(d + 1)
      ret += ChangeInstrusctions(v.before, newAfter, v.oid)
    })
    ret.toList
  }

  def intToStr(in: Int): String = {
    in.toString.length match {
      case 1 => "000" + in.toString
      case 2 => "00" + in.toString
      case 3 => "0" + in.toString
      case 4 => in.toString
      case _ => in.toString.take(4)
    }
  }


  private def getAllHull(): List[HullEspObject] = {
    import org.mongodb.scala.bson.codecs.Macros._
    val mongoDatabase: MongoDatabase = {
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[HullEspObject], classOf[PrdPart]), DEFAULT_CODEC_REGISTRY)
      val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }
    val collectionWorkShopMaterial: MongoCollection[HullEspObject] = mongoDatabase.getCollection("esp-objects-n002-hull")
    val allelems = collectionWorkShopMaterial.find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    val list: List[HullEspObject] = allelems.value.get.getOrElse(Seq.empty[HullEspObject]).toList
    list
  }

  private def getAllPipes(): List[PipeEspObject] = {
    import org.mongodb.scala.bson.codecs.Macros._
    val mongoDatabase: MongoDatabase = {
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[PipeEspObject],
        classOf[PipeSeg], classOf[Material], classOf[MaterialTranslation]), DEFAULT_CODEC_REGISTRY)
      val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }
    val collectionWorkShopMaterial: MongoCollection[PipeEspObject] = mongoDatabase.getCollection("esp-objects-n002-pipe")
    val allelems = collectionWorkShopMaterial.find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    val list: List[PipeEspObject] = allelems.value.get.getOrElse(Seq.empty[PipeEspObject]).toList
    list
  }

  private def getAllMats(): List[Material] = {
    import org.mongodb.scala.bson.codecs.Macros._
    val mongoDatabase: MongoDatabase = {
      val codecRegistry: CodecRegistry = fromRegistries(fromProviders(classOf[PipeEspObject],
        classOf[PipeSeg], classOf[Material], classOf[MaterialTranslation]), DEFAULT_CODEC_REGISTRY)
      val mongoClient: MongoClient = MongoClient("mongodb://192.168.1.26:27017")
      mongoClient.getDatabase("3degdatabase").withCodecRegistry(codecRegistry)
    }
    val collectionWorkShopMaterial: MongoCollection[Material] = mongoDatabase.getCollection("materials-n")
    val allelems = collectionWorkShopMaterial.find().toFuture()
    Await.result(allelems, Duration(100, SECONDS))
    val list: List[Material] = allelems.value.get.getOrElse(Seq.empty[Material]).toList
    list
  }


  private def UpdateStdPro(newStock: String, oid: Int) {
    ConnectionManager.connectionByProject("N002") match {
      case Some(connection) => {
        connection.setAutoCommit(false)
        val stmt: Statement = connection.createStatement()
        val sql = s"update STD_PROFILE set STOCK_CODE0='${newStock}' where OID=${oid}"
        val rs = stmt.executeUpdate(sql)
        println(rs + " " + sql)
        connection.commit()
        stmt.close()
        connection.close()
      }
    }
  }

  private def processOracle(in: List[ChangeInstrusctions]) {
    ConnectionManager.connectionByProject("N002") match {
      case Some(connection) => {
        connection.setAutoCommit(false)
        val stmt: Statement = connection.createStatement()
        in.foreach(cxh => {

          /*        components.find(s => s.stock.equals(cxh.before)) match {
                    case Some(value) => {
                      val sql = s"update component set STOCK_CODE='${cxh.after}' where OID=${value.oid}"
                      val rs = stmt.executeUpdate(sql)
                      println(rs + " " + sql)
                    }
                    case None => None
                  }
                  */

          /*       sysLangs.find(s => s.long_descr.contains(cxh.before)) match {
                   case Some(value) => {
                     val sqlVal = value.long_descr.replace(cxh.before, cxh.after)
                     val sql = s"update SYSTEMS_LANG set LONG_DESCR='${sqlVal}' where SYSTEM=${value.system} "
                     val rs = stmt.executeUpdate(sql)
                     println(rs + " " + sql)
                   }
                   case None => None
                 }
       */
          /*          pl.find(p => p.oid.toString.toUpperCase().equals(cxh.oid)) match {
                      case Some(value) => {
                        val sqlUpd = cxh.after
                        val sql = s"update STD_PLATE set STORAGE_CODE='${cxh.after}' where oid=${value.oid}"
                        val rs = stmt.executeUpdate(sql)
                        println(rs + " " + sqlUpd)
                      }
                      case None => None
                    }*/


          pro.find(p => p.stock.toUpperCase().equals(cxh.before)) match {
            case Some(value) => {
              val sqlUpd = cxh.after
              val sql = s"update STD_PROFILE set STOCK_CODE0='${cxh.after}' where oid=${value.oid}"
              val rs = stmt.executeUpdate(sql)
              println(rs + " " + sqlUpd)
            }
            case None => None
          }

        })
        connection.commit()
        stmt.close()
        connection.close()

      }
    }
  }

  private def process(in: List[ChangeInstrusctions]): Unit = {
    /*   mats.foreach(m=>{
         codesToChange.find(d => d.before.equals(m.code)) match {
           case Some(value) => {
             if(m.project=="200101"){
               newMtas += m.copy(code = value.after)
             }else{
               newMtas+=m
             }
           }
           case None => newMtas+=m
         }
       })
   */
    in.foreach(cxh => {
      /*
            components.find(s => s.stock.equals(cxh.before)) match {
              case Some(value) => {
                //println("sql update component "+ value.oid+" "+value.stock)
              }
              case None => None
            }
            sysLangs.find(s => s.long_descr.contains(cxh.before)) match {
              case Some(value) => {
                val sqlVal = value.long_descr.replace(cxh.before, cxh.after)
              }
              case None => None
            }
            pl.find(p => p.stock.toUpperCase().equals(cxh.before)) match {
              case Some(value) => {
                val sqlUpd = cxh.after
              }
              case None => None
            }
            pro.find(p => p.stock.toUpperCase().equals(cxh.before)) match {
              case Some(value) => {
                val sqlUpd = cxh.after
              }
              case None => None
            }
      */


      /*            hullDrawings.foreach(dr=>{
                    val buff=ListBuffer.empty[PrdPart]
                    dr.elements.foreach(el=>{
                      if(el.STOCK_CODE==cxh.before){
                        buff+=el.copy(STOCK_CODE = cxh.after)
                      }

                      if(el.STOCK_CODE==null) {
                         buff+=el.copy(STOCK_CODE = "")
                       }else{
                         buff+=el
                       }

                    })
                    if(buff.nonEmpty){
                      dr.elements=buff.toList
                    }
                  })*/


      /*pipesDrawings.foreach(dr=>{
        val buff=ListBuffer.empty[PipeSeg]
        dr.elements.foreach(ps=>{
          if(ps.stock==cxh.before){
            buff+=ps.copy(stock = cxh.after)
            println("pipe drawing "+ cxh.after)
          }else{
            buff+=ps
          }
        })
        if(buff.nonEmpty){
          dr.elements=buff.toList
        }
      })*/
    })


  }


}

