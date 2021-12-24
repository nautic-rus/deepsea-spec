package local.common

import io.circe.{Decoder, Encoder}
import io.circe.parser._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import local.common.DBRequests.MountItem
import local.domain.WorkShopMaterial
import local.ele.CommonEle.{EleComplect}
import local.ele.cb.CableBoxManager.ForanCableBox
import local.ele.eq.EleEqManager.EleEq
import local.ele.trays.TrayManager.{ForanTray, Tray, TrayMountData, TrayMountRules}
import local.hull.BStree.{Block, BsTreeItem, HullPL, Room}
import local.hull.PartManager.PrdPart
import local.sql.MongoDB
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.bson.types.ObjectId
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.MongoDatabase
import org.mongodb.scala.bson.codecs.Macros._

trait Codecs {




  implicit val EleComplectDecoder: Decoder[EleComplect] = deriveDecoder[EleComplect]
  implicit val EleComplectEncoder: Encoder[EleComplect] = deriveEncoder[EleComplect]

  implicit val EleEqDecoder: Decoder[EleEq] = deriveDecoder[EleEq]
  implicit val EleEqEncoder: Encoder[EleEq] = deriveEncoder[EleEq]





  implicit val WorkShopMaterialDecoder: Decoder[WorkShopMaterial] = deriveDecoder[WorkShopMaterial]
  implicit val WorkShopMaterialEncoder: Encoder[WorkShopMaterial] = deriveEncoder[WorkShopMaterial]

  implicit val TrayMountDataDecoder: Decoder[TrayMountData] = deriveDecoder[TrayMountData]
  implicit val TrayMountDataEncoder: Encoder[TrayMountData] = deriveEncoder[TrayMountData]
  implicit val ForanTrayDecoder: Decoder[ForanTray] = deriveDecoder[ForanTray]
  implicit val ForanTrayEncoder: Encoder[ForanTray] = deriveEncoder[ForanTray]
  implicit val MountItemDecoder: Decoder[MountItem] = deriveDecoder[MountItem]
  implicit val MountItemEncoder: Encoder[MountItem] = deriveEncoder[MountItem]
  implicit val TrayDecoder: Decoder[Tray] = deriveDecoder[Tray]
  implicit val TrayEncoder: Encoder[Tray] = deriveEncoder[Tray]

  implicit val ForanCableBoxDecoder: Decoder[ForanCableBox] = deriveDecoder[ForanCableBox]
  implicit val ForanCableBoxEncoder: Encoder[ForanCableBox] = deriveEncoder[ForanCableBox]

  implicit val PrdPartDecoder: Decoder[PrdPart] = deriveDecoder[PrdPart]
  implicit val PrdPartEncoder: Encoder[PrdPart] = deriveEncoder[PrdPart]

  implicit val decodeObjectId: Decoder[ObjectId] = Decoder[String].map(str => new ObjectId(str))
  implicit val encodeObjectId: Encoder[ObjectId] = Encoder[String].contramap(_.toString)

  implicit val RoomDecoder: Decoder[Room] = deriveDecoder[Room]
  implicit val RoomEncoder: Encoder[Room] = deriveEncoder[Room]

  implicit val HullPLDecoder: Decoder[HullPL] = deriveDecoder[HullPL]
  implicit val HullPLEncoder: Encoder[HullPL] = deriveEncoder[HullPL]

  implicit val BlockDecoder: Decoder[Block] = deriveDecoder[Block]
  implicit val BlockEncoder: Encoder[Block] = deriveEncoder[Block]

  private val codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[EleComplect],
    classOf[TrayMountData],
    classOf[TrayMountRules],
    classOf[HullPL],
    classOf[BsTreeItem],
    classOf[Block],
    classOf[WorkShopMaterial],
  ), DEFAULT_CODEC_REGISTRY)

  def mongoDatabase(): MongoDatabase = MongoDB.mongoClient().getDatabase("3degdatabase").withCodecRegistry(codecRegistry)


}