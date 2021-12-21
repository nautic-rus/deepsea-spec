package local.ele

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import local.sql.MongoDB
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.{MongoCollection, MongoDatabase}
import org.mongodb.scala.bson.codecs.Macros._
import org.mongodb.scala.model.Filters.equal

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, FiniteDuration, SECONDS}

import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._

object CommonEle {
  case class EleComplect(drawingId: String = "", drawingDescr: String = "", deck: String = "", project: String = "P701", systemNames: List[String] = List.empty[String], zoneNames: List[String] = List.empty[String])

  private implicit val EleComplectDecoder: Decoder[EleComplect] = deriveDecoder[EleComplect]
  private implicit val EleComplectEncoder: Encoder[EleComplect] = deriveEncoder[EleComplect]
  private val duration: FiniteDuration = Duration(2, SECONDS)
  private val codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[EleComplect]
  ), DEFAULT_CODEC_REGISTRY)

  private def mongoDatabase(): MongoDatabase = MongoDB.mongoClient().getDatabase("3degdatabase").withCodecRegistry(codecRegistry)

  private def collectionEleComplect(): MongoCollection[EleComplect] = mongoDatabase().getCollection("eleComplects")

  def retrieveEleComplects(project: String): List[EleComplect] = {
    val allelems = collectionEleComplect().find(equal("project", project)).toFuture()
    Await.result(allelems, duration)
    allelems.value.get.getOrElse(Seq.empty[EleComplect]).toList
  }

  def retrieveEleComplectsJsonString(project: String): String = {
    retrieveEleComplects(project).asJson.noSpaces
  }

}
