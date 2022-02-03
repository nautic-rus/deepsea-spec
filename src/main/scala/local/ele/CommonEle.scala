package local.ele


import local.sql.MongoDB
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoCollection, MongoDatabase}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.Filters.equal

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}

import io.circe.parser._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import local.common.Codecs
import local.common.DBRequests.MountItem
import local.domain.WorkShopMaterial
import local.ele.cb.CableBoxManager.CableBox
import local.ele.eq.EleEqManager.{EleEq, genEqsByComplect}
import local.ele.trays.TrayManager.{ForanTray, Tray, TrayMountData, traysByComplect}

import scala.io.{BufferedSource, Source}

object CommonEle extends Codecs {
  case class EleComplect(drawingId: String = "", drawingDescr: String = "", deck: String = "", project: String = "P701", systemNames: List[String] = List.empty[String], zoneNames: List[String] = List.empty[String])

  case class EleComplectParts(complect: EleComplect = EleComplect(), eqs: List[EleEq] = List.empty[EleEq], trays: List[Tray] = List.empty[Tray])

  case class Cable(SEQID: Int = 0, CODE: String = "", SPEC: Int = 0, SECT: Int = 0, SECTION: String = "",
                   MARK: String = "", SCHEME: String = "", FROM_N: String = "", FROM_N_ROOM: String = "",
                   FROM_N_X: Double = 0, FROM_N_Y: Double = 0, FROM_N_Z: Double = 0,
                   FROM_E_USERID: String = "", FROM_E_USERID_ELEC: String = "",
                   FROM_E_ROOM: String = "", FROM_E_X: Double = 0, FROM_E_Y: Double = 0, FROM_E_Z: Double = 0,
                   TO_E_USERID: String = "", TO_N: String = "", TO_N_ROOM: String = "",
                   TO_N_X: Double = 0, TO_N_Y: Double = 0, TO_N_Z: Double = 0, TO_E_USERID_ELEC: String = "",
                   TO_E_ROOM: String = "", TO_E_X: Double = 0, TO_E_Y: Double = 0, TO_E_Z: Double = 0, LEN: Double = 0, CABLEPATH: String = "",
                   ADDDATA: String = "", PARTNUMBER: String = "", STOCKCODE: String = "", complectName: String = "", ws: WorkShopMaterial = new WorkShopMaterial())

  private val duration: FiniteDuration = Duration(5, SECONDS)

  private def collectionEleComplect(): MongoCollection[EleComplect] = mongoDatabase().getCollection("eleComplects")

  def retrieveEleComplects(project: String): List[EleComplect] = {
    val allelems = collectionEleComplect().find(equal("project", project)).toFuture()
    Await.result(allelems, duration)
    allelems.value.get.getOrElse(Seq.empty[EleComplect]).toList
  }

  def retrieveEleComplectsJsonString(project: String): String = {
    retrieveEleComplects(project).asJson.noSpaces
  }

  def retrieveAllPartsByComplectName(project: String, complectName: String): EleComplectParts = {
    retrieveEleComplects(project).find(s => s.drawingId.equals(complectName)) match {
      case Some(complect: EleComplect) => {
        val trays: List[Tray] = traysByComplect(project, complect)
        val eqs: List[EleEq] = genEqsByComplect(project, complect)
        EleComplectParts(complect, eqs, trays)
      }
      case None => EleComplectParts()
    }
  }

  def retrieveAllPartsByComplectNameJSON(project: String, complectName: String): String = retrieveAllPartsByComplectName(project, complectName).asJson.noSpaces

  def retrieveAllPartsFromJSON(jsonPath: String): EleComplectParts = {
    val src: BufferedSource = Source.fromFile(jsonPath)
    val fileContents: String = src.getLines.mkString
    src.close()
    decode[EleComplectParts](fileContents) match {
      case Right(value) => value
      case Left(value) => EleComplectParts()
    }
  }

}
