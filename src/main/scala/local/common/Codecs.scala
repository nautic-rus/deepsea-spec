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
import local.domain.CommonTypes.{DrawingChess, DrawingChessItem, ZoneSystem}
import local.domain.WorkShopMaterial
import local.ele.CommonEle.{Cable, EleComplect}
import local.ele.cb.CableBoxManager.ForanCableBox
import local.ele.cbfill.models.{AnkerPlate, CableBoxModule, CompressionBlock, SealModule}
import local.ele.trays.TrayManager.{ForanTray, Tray, TrayMountData, TrayMountRules}
import local.ele.eq.EleEqManager.EleEq
import local.hull.BStree.{Block, BsTreeItem, HullPL, Room}
import local.hull.PartManager.PrdPart
import local.hull.nest.CommonNest.{Nest, NestMaterial}
import local.pdf.ru.common.ReportCommon.Item11Columns
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

  implicit val Item11ColumnsDecoder: Decoder[Item11Columns] = deriveDecoder[Item11Columns]
  implicit val Item11ColumnsEncoder: Encoder[Item11Columns] = deriveEncoder[Item11Columns]

  implicit val DrawingChessItemDecoder: Decoder[DrawingChessItem] = deriveDecoder[DrawingChessItem]
  implicit val DrawingChessItemEncoder: Encoder[DrawingChessItem] = deriveEncoder[DrawingChessItem]

  implicit val DrawingChessDecoder: Decoder[DrawingChess] = deriveDecoder[DrawingChess]
  implicit val DrawingChessEncoder: Encoder[DrawingChess] = deriveEncoder[DrawingChess]

  implicit val CableDecoder: Decoder[Cable] = deriveDecoder[Cable]
  implicit val CableEncoder: Encoder[Cable] = deriveEncoder[Cable]

  implicit val ZoneSystemDecoder: Decoder[ZoneSystem] = deriveDecoder[ZoneSystem]
  implicit val ZoneSystemEncoder: Encoder[ZoneSystem] = deriveEncoder[ZoneSystem]

  implicit val NestDecoder: Decoder[Nest] = deriveDecoder[Nest]
  implicit val NestEncoder: Encoder[Nest] = deriveEncoder[Nest]

  implicit val NestMaterialDecoder: Decoder[NestMaterial] = deriveDecoder[NestMaterial]
  implicit val NestMaterialEncoder: Encoder[NestMaterial] = deriveEncoder[NestMaterial]

  private val codecRegistry: CodecRegistry = fromRegistries(fromProviders(
    classOf[EleComplect],
    classOf[TrayMountData],
    classOf[TrayMountRules],
    classOf[HullPL],
    classOf[BsTreeItem],
    classOf[Block],
    classOf[WorkShopMaterial],
    classOf[DrawingChess],
    classOf[DrawingChessItem],
    classOf[SealModule],
    classOf[CompressionBlock],
    classOf[AnkerPlate],
    classOf[CableBoxModule],
  ), DEFAULT_CODEC_REGISTRY)

  def mongoDatabase(): MongoDatabase = MongoDB.mongoClient().getDatabase("3degdatabase").withCodecRegistry(codecRegistry)


}
