package deepsea.database

import deepsea.hull.HullManager.PlatePart
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros._

trait MongoCodecs {

  implicit val MaterialDecoder: Decoder[PlatePart] = deriveDecoder[PlatePart]
  implicit val MaterialEncoder: Encoder[PlatePart] = deriveEncoder[PlatePart]


  def codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[PlatePart],
  ), DEFAULT_CODEC_REGISTRY)
}
