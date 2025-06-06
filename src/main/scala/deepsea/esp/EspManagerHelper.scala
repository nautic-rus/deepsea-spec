package deepsea.esp

import akka.pattern.ask
import com.mongodb.client.model.BsonField
import deepsea.actors.ActorManager
import deepsea.database.DBManager
import deepsea.esp.EspManager.{DeviceEspObject, DocumentWithMaterial, EleEspObject, EspElement, EspHistoryObject, EspObject, GlobalEsp, GlobalEspSpec, HullEspObject, IssueProject, MaterialPurchase, MaterialSummary, PipeEspObject, espKinds, espObjectsCollectionName}
import deepsea.files.FileManager
import deepsea.files.FileManager.GenerateUrl
import deepsea.materials.MaterialManager.{getMaterialStatements, getSpecMaterials}
import deepsea.materials.MaterialsHelper
import deepsea.pipe.PipeManager.{Material, Units}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import local.pdf.en.accom.AccomReportEn.getUnits
import local.pdf.ru.common.ReportCommon.Item11Columns
import local.pdf.ru.order.OrderReportV1
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

trait EspManagerHelper extends Codecs with MaterialsHelper{

  def addHullEsp(esp: HullEspObject): Unit ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, esp.foranProject, "hull").mkString("-").toLowerCase
        val espCollection: MongoCollection[HullEspObject] = mongo.getCollection(espCollectionName)
        Await.result(espCollection.insertOne(esp).toFuture(), Duration(60, SECONDS))
      case _ => None
    }
  }
  def addPipeEsp(esp: PipeEspObject): Unit ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, esp.foranProject, "pipe").mkString("-").toLowerCase
        val espCollection: MongoCollection[PipeEspObject] = mongo.getCollection(espCollectionName)
        Await.result(espCollection.insertOne(esp).toFuture(), Duration(10, SECONDS))
      case _ => None
    }
  }
  def addDevicesEsp(esp: DeviceEspObject): Unit = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, esp.foranProject, "device").mkString("-").toLowerCase
        val espCollection: MongoCollection[DeviceEspObject] = mongo.getCollection(espCollectionName)
        Await.result(espCollection.insertOne(esp).toFuture(), Duration(10, SECONDS))
      case _ => None
    }
  }
  def addEleEsp(esp: EleEspObject): Unit = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, esp.foranProject, "ele").mkString("-").toLowerCase
        val espCollection: MongoCollection[EleEspObject] = mongo.getCollection(espCollectionName)
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
  def getPipeLatestEsp(foranProject: String, kind: String, docNumber: String, rev: String): Option[PipeEspObject] = {
    DBManager.GetMongoNewConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, foranProject, kind).mkString("-").toLowerCase
        val espCollection: MongoCollection[PipeEspObject] = mongo.getCollection(espCollectionName)
        if (rev == "") {
          Await.result(espCollection.find(and(equal("docNumber", docNumber))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
            case espObject: PipeEspObject => Option(espObject)
            case _ => Option.empty[PipeEspObject]
          }
        }
        else {
          Await.result(espCollection.find(and(equal("docNumber", docNumber), equal("rev", rev))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
            case espObject: PipeEspObject => Option(espObject)
            case _ => Option.empty[PipeEspObject]
          }
        }
      case _ => None
    }
  }
  def getDeviceLatestEsp(foranProject: String, kind: String, docNumber: String, rev: String): Option[DeviceEspObject] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, foranProject, kind).mkString("-").toLowerCase
        val espCollection: MongoCollection[DeviceEspObject] = mongo.getCollection(espCollectionName)
        if (rev == "") {
          Await.result(espCollection.find(and(equal("docNumber", docNumber))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
            case espObject: DeviceEspObject => Option(espObject)
            case _ => Option.empty[DeviceEspObject]
          }
        }
        else {
          Await.result(espCollection.find(and(equal("docNumber", docNumber), equal("rev", rev))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
            case espObject: DeviceEspObject => Option(espObject)
            case _ => Option.empty[DeviceEspObject]
          }
        }
      case _ => None
    }
  }
  def getEleLatestEsp(foranProject: String, kind: String, docNumber: String, rev: String): Option[EleEspObject] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollectionName = List(espObjectsCollectionName, foranProject, kind).mkString("-").toLowerCase
        val espCollection: MongoCollection[EleEspObject] = mongo.getCollection(espCollectionName)
        if (rev == "") {
          Await.result(espCollection.find(and(equal("docNumber", docNumber))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
            case espObject: EleEspObject => Option(espObject)
            case _ => Option.empty[EleEspObject]
          }
        }
        else {
          Await.result(espCollection.find(and(equal("docNumber", docNumber), equal("rev", rev))).sort(descending("date")).first().toFuture(), Duration(10, SECONDS)) match {
            case espObject: EleEspObject => Option(espObject)
            case _ => Option.empty[EleEspObject]
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

  def getHullAllLatestEsp(projects: List[String] = List("N002", "N004", "N008")): List[HullEspObject] ={
    val res = ListBuffer.empty[HullEspObject]
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        projects.foreach(project => {
          val espCollectionName = List(espObjectsCollectionName, project, "hull").mkString("-").toLowerCase
          val espCollection: MongoCollection[HullEspObject] = mongo.getCollection(espCollectionName)
          try {
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
                  model.BsonField("taskId", Document("$last" -> "$taskId")),
                  model.BsonField("elements", Document("$last" -> "$elements")),
                )
              )).allowDiskUse(true).toFuture(), Duration(60, SECONDS)) match {
              case espObjects: Seq[HullEspObject] => res ++= espObjects.toList
              case _ => None
            }
          }
          catch {
            case e: Exception => println(e.toString)
          }
        })
      case _ => None
    }
    res.toList
  }
  def getPipeAllLatestEsp(projects: List[String] = List("N002", "N004", "N008")): List[PipeEspObject] ={
    val res = ListBuffer.empty[PipeEspObject]
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        projects.foreach(project => {
          val espCollectionName = List(espObjectsCollectionName, project, "pipe").mkString("-").toLowerCase
          val espCollection: MongoCollection[PipeEspObject] = mongo.getCollection(espCollectionName)
          try {
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
                  model.BsonField("taskId", Document("$last" -> "$taskId")),
                  model.BsonField("elements", Document("$last" -> "$elements")),
                )
              )).allowDiskUse(true).toFuture(), Duration(60, SECONDS)) match {
              case espObjects: Seq[PipeEspObject] => res ++= espObjects.toList
              case _ => None
            }
          }
          catch {
            case e: Exception => println(e.toString)
          }
        })
      case _ => None
    }
    res.toList
  }
  def getDeviceAllLatestEsp(projects: List[String] = List("N002", "N004", "N008")): List[DeviceEspObject] = {
    val res = ListBuffer.empty[DeviceEspObject]
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        projects.foreach(project => {
          val espCollectionName = List(espObjectsCollectionName, project, "device").mkString("-").toLowerCase
          val espCollection: MongoCollection[DeviceEspObject] = mongo.getCollection(espCollectionName)
          try {
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
                  model.BsonField("taskId", Document("$last" -> "$taskId")),
                  model.BsonField("elements", Document("$last" -> "$elements")),
                )
              )).allowDiskUse(true).toFuture(), Duration(60, SECONDS)) match {
              case espObjects: Seq[DeviceEspObject] => res ++= espObjects.toList
              case _ => None
            }
          }
          catch {
            case e: Exception => println(e.toString)
          }
        })
      case _ => None
    }
    res.toList
  }
  def getEleAllLatestEsp(projects: List[String] = List("N002", "N004", "N008")): List[EleEspObject] = {
    val res = ListBuffer.empty[EleEspObject]
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        projects.foreach(project => {
          val espCollectionName = List(espObjectsCollectionName, project, "ele").mkString("-").toLowerCase
          val espCollection: MongoCollection[EleEspObject] = mongo.getCollection(espCollectionName)
          try {
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
                  model.BsonField("taskId", Document("$last" -> "$taskId")),
                  model.BsonField("elements", Document("$last" -> "$elements")),
                )
              )).allowDiskUse(true).toFuture(), Duration(60, SECONDS)) match {
              case espObjects: Seq[EleEspObject] => res ++= espObjects.toList
              case _ => None
            }
          }
          catch {
            case e: Exception => println(e.toString)
          }
        })
      case _ => None
    }
    res.toList
  }

  def generateGlobalEsp(projects: List[String]): String = {
    (generateHullGlobalEsp(projects) ++ generatePipeGlobalEsp(projects) ++ generateDeviceGlobalEsp(projects)).asJson.noSpaces
  }
  def getGlobalEsp(projectId: Int): List[GlobalEsp] = {
    val projects = getIssueProjects.find(_.id == projectId) match {
      case Some(value) => List(value.foran)
      case _ => List.empty[String]
    }
    (generateHullGlobalEsp(projects) ++ generatePipeGlobalEsp(projects) ++ generateDeviceGlobalEsp(projects) ++ generateEleGlobalEsp(projects))
  }

  def generateHullGlobalEsp(projects: List[String]): List[GlobalEsp] ={
    val res = ListBuffer.empty[GlobalEsp]
    val units: List[Units] = getUnits
    val materials = getMaterials
    projects.foreach(p => {
      val esps = getHullAllLatestEsp(List(p))
      val elems = esps.flatMap(_.elements)
      elems.filter(_.STOCK_CODE != "").groupBy(x => x.STOCK_CODE).map(group => {
        val qty = group._2.map(_.QTY).sum
        val weight = group._2.head.WEIGHT_UNIT
        val weightTotal = group._2.map(_.TOTAL_WEIGHT).sum
        val material = materials.find(_.code == group._1) match {
          case Some(value) => value
          case _ => Material()
        }
        val name = List(group._2.head.PART_TYPE, group._2.head.PART_DESC, group._2.head.THICKNESS, group._2.head.MATERIAL).mkString(", ")

        val docMaterial = ListBuffer.empty[DocumentWithMaterial]
        esps.foreach(esp => {
          esp.elements.filter(x => x.STOCK_CODE == group._1).foreach(pos => {
            docMaterial += DocumentWithMaterial(
              esp.docNumber,
              esp.rev,
              esp.user,
              esp.date,
              "kg",
              "166",
              pos.TOTAL_WEIGHT,
              pos.WEIGHT_UNIT,
              pos.TOTAL_WEIGHT,
              pos.PART_CODE
            )
          })
        })
        res += GlobalEsp(
          group._1,
          (if (material.name != "") material.name else name),
          material.description,
          units.find(_.code == material.units) match {
            case Some(value) => value.thumb
            case _ => material.units
          },
          "166",
          weightTotal.formatted("%.2f").toDouble,
          0,
          weightTotal.formatted("%.2f").toDouble,
          docMaterial.toList,
          material
        )
      })
    })
    res.toList.filter(_.code != "")
  }
  def generateHullGlobalEspAsData(projects: List[String]): List[MaterialSummary] ={
    val res = ListBuffer.empty[MaterialSummary]
    projects.foreach(p => {
      val esps = getHullAllLatestEsp(List(p))
      val elems = esps.flatMap(_.elements)
      elems.groupBy(x => (x.ELEM_TYPE, x.THICKNESS, x.WIDTH, x.MATERIAL)).map(group => {
        val qty = group._2.map(_.QTY).sum
        val weight = group._2.head.WEIGHT_UNIT
        val weightTotal = group._2.map(_.TOTAL_WEIGHT).sum
        res += MaterialSummary(
          Material(),
          group._1._1,
          List(group._1._2, group._1._3.toString).mkString(","),
          group._1._4,
          qty,
          weight,
          weightTotal,
          List.empty[String]
        )
      })
    })
    res.toList
  }

  def generatePipeGlobalEsp(projects: List[String]): List[GlobalEsp] ={
    val res = ListBuffer.empty[GlobalEsp]
    val materials = getMaterials
    val units: List[Units] = getUnits
    projects.foreach(p => {
      val esps = getPipeAllLatestEsp(List(p))
      val elems = esps.flatMap(_.elements).filter(_.stock != "")
      elems.filter(_.spool != "").groupBy(x => x.stock).map(group => {
        val material = materials.find(_.code == group._1) match {
          case Some(value) => value
          case _ => Material()
        }
        val qty = material.units match {
          case "796" => group._2.length
          case "006" => group._2.map(_.length).sum / 1000
          case "166" => group._2.map(_.weight).sum
          case _ => group._2.length
        }
        val weight = material.units match {
          case _ => material.singleWeight
        }
        val weightTotal = material.units match {
          case "796" => qty * material.singleWeight
          case _ => group._2.map(_.weight).sum
        }

        val docMaterial = ListBuffer.empty[DocumentWithMaterial]
        esps.foreach(esp => {
          esp.elements.filter(x => x.stock == material.code && x.spool != "").foreach(pos => {
            docMaterial += DocumentWithMaterial(
              esp.docNumber,
              esp.rev,
              esp.user,
              esp.date,
              units.find(_.code == material.units) match {
                case Some(value) => value.thumb
                case _ => material.units
              },
              pos.material.units,
              material.units match {
                case "796" => 1
                case "006" => pos.length / 1000
                case "166" => pos.weight
                case _ => group._2.length
              },
              material.singleWeight,
              pos.weight,
              if (pos.spool != ""){
                pos.spool + "." + pos.spPieceId
              }
              else{
                ""
              }
            )
          })
        })

        res += GlobalEsp(
          material.code,
          material.name("ru"),
          material.name,
          units.find(_.code == material.units) match {
            case Some(value) => value.thumb
            case _ => material.units
          },
          material.units,
          qty.formatted("%.2f").toDouble,
          weight.formatted("%.2f").toDouble,
          weightTotal.formatted("%.2f").toDouble,
          docMaterial.toList,
          material
        )

//        res += Item11Columns(
//          isHeader = false,
//          material.code,
//          material.name("ru"),
//          material.name,
//          units.find(_.code == material.units) match {
//            case Some(value) => value.thumb
//            case _ => material.units
//          },
//          qty.formatted("%.2f"),
//          weight.formatted("%.2f"),
//          weightTotal.formatted("%.2f"),
//          esps.filter(_.elements.exists(_.material.code == material.code)).map(_.docNumber).mkString(",")
//        )
      })
    })
    res.toList.filter(_.code != "")
  }
  def generatePipeGlobalEspAsData(projects: List[String]): List[MaterialSummary] ={
    val res = ListBuffer.empty[MaterialSummary]
    val materials = getMaterials
    val units: List[Units] = getUnits
    projects.foreach(p => {
      val esps = getPipeAllLatestEsp(List(p))
      val elems = esps.flatMap(_.elements).filter(_.material.code != "")
      elems.groupBy(x => (x.material.code)).map(group => {
        val material = materials.find(_.code == group._1) match {
          case Some(value) => value
          case _ => Material()
        }
        val qty = material.units match {
          case "796" => group._2.length
          case "006" => group._2.map(_.length).sum
          case "166" => group._2.map(_.weight).sum
          case _ => group._2.length
        }
        val weight = material.units match {
          case _ => material.singleWeight
        }
        val weightTotal = material.units match {
          case "796" => qty * material.singleWeight
          case _ => group._2.map(_.weight).sum
        }
        res += MaterialSummary(
          material,
          material.name,
          material.description,
          units.find(_.code == material.units) match {
            case Some(value) => value.thumb
            case _ => material.units
          },
          qty,
          weight,
          weightTotal,
          esps.filter(_.elements.exists(_.material.code == material.code)).map(_.docNumber)
        )
      })
    })
    res.toList
  }

  def generateDeviceGlobalEsp(projects: List[String]): List[GlobalEsp] = {
    val res = ListBuffer.empty[GlobalEsp]
    val materials = getMaterials
    val units: List[Units] = getUnits
    projects.foreach(p => {
      val esps = getDeviceAllLatestEsp(List(p))
      val elems = esps.flatMap(_.elements).filter(_.material.code != "")
      elems.groupBy(_.material.code).map(group => {
        val material = materials.find(_.code == group._1) match {
          case Some(value) => value
          case _ => Material()
        }
        val qty = material.units match {
          case "796" => group._2.map(_.count).sum
          case "006" => group._2.map(_.count).sum
          case "166" => group._2.map(_.weight).sum
          case "055" => group._2.map(_.weight).sum / material.singleWeight
          case _ => group._2.length
        }
        val weight = material.units match {
          case _ => material.singleWeight
        }
        val weightTotal = material.units match {
          case "796" => qty * material.singleWeight
          case "006" => qty * material.singleWeight
          case _ => group._2.map(_.weight).sum
        }

        val docMaterial = ListBuffer.empty[DocumentWithMaterial]
        esps.foreach(esp => {
          esp.elements.filter(_.material.code == group._1).foreach(pos => {
            docMaterial += DocumentWithMaterial(
              esp.docNumber,
              esp.rev,
              esp.user,
              esp.date,
              units.find(_.code == material.units) match {
                case Some(value) => value.thumb
                case _ => material.units
              },
              pos.material.units,
              material.units match {
                case "796" => if (pos.count != 0) pos.count else 1
                case "006" => pos.weight
                case "166" => pos.weight
                case _ => group._2.length
              },
              material.singleWeight,
              pos.weight,
              pos.userId,
            )
          })
        })

        res += GlobalEsp(
          material.code,
          material.name("ru"),
          material.name,
          units.find(_.code == material.units) match {
            case Some(value) => value.thumb
            case _ => material.units
          },
          material.units,
          qty.formatted("%.2f").toDouble,
          weight.formatted("%.3f").toDouble,
          weightTotal.formatted("%.3f").toDouble,
          docMaterial.toList,
          material
        )
      })
    })
    res.toList.filter(_.code != "")
  }
  def generateEleGlobalEsp(projects: List[String]): List[GlobalEsp] = {
    val res = ListBuffer.empty[GlobalEsp]
    val materials = getMaterials
    val units: List[Units] = getUnits
    projects.foreach(p => {
      val esps = getEleAllLatestEsp(List(p))
      val elems = esps.flatMap(_.elements).filter(_.material.code != "")
      elems.groupBy(_.material.code).map(group => {
        val material = materials.find(_.code == group._1) match {
          case Some(value) => value
          case _ => Material()
        }
        if (material.code == "ESNCARCTSXXX0011"){
          val q = 0
        }
        val qty = material.units match {
          case "796" => group._2.map(_.count).sum
          case "006" => group._2.map(_.count).sum
          case "166" => group._2.map(_.weight).sum
          case "055" => group._2.map(_.weight).sum / material.singleWeight
          case _ => group._2.length
        }
        val weight = material.units match {
          case _ => material.singleWeight
        }
        val weightTotal = material.units match {
          case "796" => qty * material.singleWeight
          case "006" => qty * material.singleWeight
          case _ => group._2.map(_.weight).sum
        }

        val docMaterial = ListBuffer.empty[DocumentWithMaterial]
        esps.foreach(esp => {
          esp.elements.filter(_.material.code == group._1).foreach(pos => {
            docMaterial += DocumentWithMaterial(
              esp.docNumber,
              esp.rev,
              esp.user,
              esp.date,
              units.find(_.code == material.units) match {
                case Some(value) => value.thumb
                case _ => material.units
              },
              pos.material.units,
              material.units match {
                case "796" => if (pos.count != 0) pos.count else 1
                case "006" => pos.weight
                case "166" => pos.weight
                case _ => group._2.length
              },
              material.singleWeight,
              pos.weight,
              pos.userId,
            )
          })
        })

        res += GlobalEsp(
          material.code,
          material.name("ru"),
          material.name,
          units.find(_.code == material.units) match {
            case Some(value) => value.thumb
            case _ => material.units
          },
          material.units,
          qty.formatted("%.2f").toDouble,
          weight.formatted("%.3f").toDouble,
          weightTotal.formatted("%.3f").toDouble,
          docMaterial.toList,
          material
        )
      })
    })
    res.toList.filter(_.code != "")
  }


  def addMaterialPurchase(materialPurchase: MaterialPurchase): Unit ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val collectionName = "material-purchases"
        val collection: MongoCollection[MaterialPurchase] = mongo.getCollection(collectionName)
        Await.result(collection.insertOne(materialPurchase).toFuture(), Duration(60, SECONDS))
      case _ => None
    }
  }
  def getMaterialPurchases(project: String): List[MaterialPurchase] ={
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val collectionName = "material-purchases"
        val collection: MongoCollection[MaterialPurchase] = mongo.getCollection(collectionName)
        Await.result(collection.find(equal("project", project)).toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[MaterialPurchase] => values.toList
          case _ => List.empty[MaterialPurchase]
        }
      case _ => List.empty[MaterialPurchase]
    }
  }
  def getIssueProjects: ListBuffer[IssueProject] ={
    val res = ListBuffer.empty[IssueProject]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_projects where status = 0 order by id")
        while (rs.next()){
          res += IssueProject(
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getString("name")).getOrElse(""),
            Option(rs.getString("pdsp")).getOrElse(""),
            Option(rs.getString("rkd")).getOrElse(""),
            Option(rs.getString("foran")).getOrElse(""),
            Option(rs.getString("managers")).getOrElse(""),
            Option(rs.getString("status")).getOrElse(""),
            Option(rs.getString("factory")).getOrElse(""),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }

  def getSummaryMaterials(projectId: Int): List[GlobalEspSpec] = {
    val globalEsp = getGlobalEsp(projectId)
    val grouped = ListBuffer.empty[GlobalEsp]
    globalEsp.groupBy(_.code).foreach(gr => {
      val qtySum = gr._2.map(_.qty).sum
      val weightSum = gr._2.map(_.weightTotal).sum
      val docs = gr._2.flatMap(_.documents)
      grouped += gr._2.head.copy(qty = qtySum, weightTotal = weightSum, documents = docs)
    })
    val stmts = getMaterialStatements.filter(x => x.project_id == projectId)
    val stmtsIds = stmts.map(_.id)
    val materials = getSpecMaterials.filter(x => stmtsIds.contains(x.statem_id))
    grouped.flatMap(inMaterial => {
      materials.find(_.code == inMaterial.code) match {
        case Some(sMaterial) =>
          Option(GlobalEspSpec(
            inMaterial.code,
            sMaterial.name,
            sMaterial.descr,
            inMaterial.units,
            inMaterial.unitsValue,
            inMaterial.qty,
            sMaterial.weight,
            inMaterial.weightTotal,
            inMaterial.documents,
            sMaterial
          ))
        case _ => Option.empty[GlobalEspSpec]
      }
    }).toList
  }

}
