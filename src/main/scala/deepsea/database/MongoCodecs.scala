package deepsea.database

import deepsea.hull.HullManager._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros._

trait MongoCodecs {

  implicit val PlatePartDecoder: Decoder[PlatePart] = deriveDecoder[PlatePart]
  implicit val PlatePartEncoder: Encoder[PlatePart] = deriveEncoder[PlatePart]

  implicit val ProfilePartDecoder: Decoder[ProfilePart] = deriveDecoder[ProfilePart]
  implicit val ProfilePartEncoder: Encoder[ProfilePart] = deriveEncoder[ProfilePart]



  def codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[PlatePart],
    classOf[ProfilePart],
  ), DEFAULT_CODEC_REGISTRY)
}
