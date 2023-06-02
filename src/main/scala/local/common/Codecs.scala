package local.common

import deepsea.accomodations.AccommodationManager.{Accommodation, BBox}
import deepsea.devices.DeviceManager.{Device, DeviceAux}
import deepsea.elec.ElecManager.{CableBoxesBySystem, CableRoute, ElecAngle, ElecCable, EquipmentConnection, TrayBySystem}
import deepsea.esp.EspManager.{DeviceEspObject, DocumentWithMaterial, EspElement, EspObject, GlobalEsp, HullEspObject, MaterialPurchase, PipeEspObject}
import deepsea.hull.HullManager._
import deepsea.pipe.PipeManager.{Material, MaterialTranslation, PipeSeg, PipeSegActual, PipeSegBilling, ProjectName, SpoolLock, SystemDef, UnitTranslation, Units}
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
import local.common.DBRequests.{MaterialNode, MountItem}
import local.domain.CommonTypes.{DrawingChess, DrawingChessItem, ZoneSystem}
import local.domain.WorkShopMaterial
import local.ele.CommonEle.{Cable, EleComplect}
import local.ele.cb.CableBoxManager.ForanCableBox
import local.ele.cbfill.models.{AnkerPlate, CableBoxModule, CompressionBlock, SealModule}
import local.ele.trays.TrayManager.{ForanTray, Tray, TrayMountData, TrayMountRules}
import local.ele.eq.EleEqManager.EleEq
import local.eqmount.EqFoundationManager.EqFoundation
import local.hull.BStree.{Block, BsTreeItem, HullPL, Room}
import local.hull.PartManager.PrdPart
import local.hull.bill.BillManager.{ForanScrap, PlateAnalitic, ProfileAnalitic, ProfileNestBill}
import local.hull.nest.CommonNest.{Nest, Nest2, NestLock, NestMaterial}
import local.pdf.ru.common.ReportCommon.Item11Columns
import local.sql.MongoDB
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.bson.types.ObjectId
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.MongoDatabase
import org.mongodb.scala.bson.codecs.Macros._

trait Codecs {

  //BOGDAN

  implicit val PlatePartDecoder: Decoder[PlatePart] = deriveDecoder[PlatePart]
  implicit val PlatePartEncoder: Encoder[PlatePart] = deriveEncoder[PlatePart]

  implicit val ProfilePartDecoder: Decoder[ProfilePart] = deriveDecoder[ProfilePart]
  implicit val ProfilePartEncoder: Encoder[ProfilePart] = deriveEncoder[ProfilePart]


  implicit val BsDesignNodeDecoder: Decoder[BsDesignNode] = deriveDecoder[BsDesignNode]
  implicit val BsDesignNodeEncoder: Encoder[BsDesignNode] = deriveEncoder[BsDesignNode]

  implicit val HullSystemDecoder: Decoder[HullSystem] = deriveDecoder[HullSystem]
  implicit val HullSystemEncoder: Encoder[HullSystem] = deriveEncoder[HullSystem]

  implicit val PipeSegDecoder: Decoder[PipeSeg] = deriveDecoder[PipeSeg]
  implicit val PipeSegEncoder: Encoder[PipeSeg] = deriveEncoder[PipeSeg]

  implicit val PipeSegActualDecoder: Decoder[PipeSegActual] = deriveDecoder[PipeSegActual]
  implicit val PipeSegActualEncoder: Encoder[PipeSegActual] = deriveEncoder[PipeSegActual]

  implicit val MaterialDecoder: Decoder[Material] = deriveDecoder[Material]
  implicit val MaterialEncoder: Encoder[Material] = deriveEncoder[Material]

  implicit val ProjectNameDecoder: Decoder[ProjectName] = deriveDecoder[ProjectName]
  implicit val ProjectNameEncoder: Encoder[ProjectName] = deriveEncoder[ProjectName]

  implicit val SystemDefDecoder: Decoder[SystemDef] = deriveDecoder[SystemDef]
  implicit val SystemDefEncoder: Encoder[SystemDef] = deriveEncoder[SystemDef]

  implicit val PipeSegBillingDecoder: Decoder[PipeSegBilling] = deriveDecoder[PipeSegBilling]
  implicit val PipeSegBillingEncoder: Encoder[PipeSegBilling] = deriveEncoder[PipeSegBilling]

  implicit val SpoolLockDecoder: Decoder[SpoolLock] = deriveDecoder[SpoolLock]
  implicit val SpoolLockEncoder: Encoder[SpoolLock] = deriveEncoder[SpoolLock]

  implicit val DeviceDecoder: Decoder[Device] = deriveDecoder[Device]
  implicit val DeviceEncoder: Encoder[Device] = deriveEncoder[Device]

  implicit val MaterialTranslationDecoder: Decoder[MaterialTranslation] = deriveDecoder[MaterialTranslation]
  implicit val MaterialTranslationEncoder: Encoder[MaterialTranslation] = deriveEncoder[MaterialTranslation]

  implicit val AccommodationDecoder: Decoder[Accommodation] = deriveDecoder[Accommodation]
  implicit val AccommodationEncoder: Encoder[Accommodation] = deriveEncoder[Accommodation]

  implicit val UnitsDecoder: Decoder[Units] = deriveDecoder[Units]
  implicit val UnitsEncoder: Encoder[Units] = deriveEncoder[Units]

  implicit val UnitTranslationDecoder: Decoder[UnitTranslation] = deriveDecoder[UnitTranslation]
  implicit val UnitTranslationEncoder: Encoder[UnitTranslation] = deriveEncoder[UnitTranslation]

  implicit val BBoxDecoder: Decoder[BBox] = deriveDecoder[BBox]
  implicit val BBoxEncoder: Encoder[BBox] = deriveEncoder[BBox]

  implicit val HullEspObjectDecoder: Decoder[HullEspObject] = deriveDecoder[HullEspObject]
  implicit val HullEspObjectEncoder: Encoder[HullEspObject] = deriveEncoder[HullEspObject]

  implicit val PipeEspObjectDecoder: Decoder[PipeEspObject] = deriveDecoder[PipeEspObject]
  implicit val PipeEspObjectEncoder: Encoder[PipeEspObject] = deriveEncoder[PipeEspObject]

  implicit val DeviceEspObjectDecoder: Decoder[DeviceEspObject] = deriveDecoder[DeviceEspObject]
  implicit val DeviceEspObjectEncoder: Encoder[DeviceEspObject] = deriveEncoder[DeviceEspObject]

  implicit val MaterialPurchaseDecoder: Decoder[MaterialPurchase] = deriveDecoder[MaterialPurchase]
  implicit val MaterialPurchaseEncoder: Encoder[MaterialPurchase] = deriveEncoder[MaterialPurchase]

  implicit val GlobalEspDecoder: Decoder[GlobalEsp] = deriveDecoder[GlobalEsp]
  implicit val GlobalEspEncoder: Encoder[GlobalEsp] = deriveEncoder[GlobalEsp]

  implicit val DocumentWithMaterialDecoder: Decoder[DocumentWithMaterial] = deriveDecoder[DocumentWithMaterial]
  implicit val DocumentWithMaterialEncoder: Encoder[DocumentWithMaterial] = deriveEncoder[DocumentWithMaterial]


  implicit val ElecAngleDecoder: Decoder[ElecAngle] = deriveDecoder[ElecAngle]
  implicit val ElecAngleEncoder: Encoder[ElecAngle] = deriveEncoder[ElecAngle]



  //KOKOVIN

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

  implicit val ProfileAnaliticDecoder: Decoder[ProfileAnalitic] = deriveDecoder[ProfileAnalitic]
  implicit val ProfileAnaliticEncoder: Encoder[ProfileAnalitic] = deriveEncoder[ProfileAnalitic]

  implicit val PlateAnaliticDecoder: Decoder[PlateAnalitic] = deriveDecoder[PlateAnalitic]
  implicit val PlateAnaliticEncoder: Encoder[PlateAnalitic] = deriveEncoder[PlateAnalitic]

  implicit val NestLockDecoder: Decoder[NestLock] = deriveDecoder[NestLock]
  implicit val NestLockEncoder: Encoder[NestLock] = deriveEncoder[NestLock]

  implicit val ForanScrapecoder: Decoder[ForanScrap] = deriveDecoder[ForanScrap]
  implicit val ForanScrapEncoder: Encoder[ForanScrap] = deriveEncoder[ForanScrap]

  implicit val Nest2Decoder: Decoder[Nest2] = deriveDecoder[Nest2]
  implicit val Nest2Encoder: Encoder[Nest2] = deriveEncoder[Nest2]

  implicit val ProfileNestBillDecoder: Decoder[ProfileNestBill] = deriveDecoder[ProfileNestBill]
  implicit val ProfileNestBillEncoder: Encoder[ProfileNestBill] = deriveEncoder[ProfileNestBill]

  implicit val EqFoundationDecoder: Decoder[EqFoundation] = deriveDecoder[EqFoundation]
  implicit val EqFoundationEncoder: Encoder[EqFoundation] = deriveEncoder[EqFoundation]

  implicit val ElecCableDecoder: Decoder[ElecCable] = deriveDecoder[ElecCable]
  implicit val ElecCableEncoder: Encoder[ElecCable] = deriveEncoder[ElecCable]

  // MAMONOV

  implicit val TrayBySystemDecoder: Decoder[TrayBySystem] = deriveDecoder[TrayBySystem]
  implicit val TrayBySystemEncoder: Encoder[TrayBySystem] = deriveEncoder[TrayBySystem]

  implicit val CableBoxesBySystemDecoder: Decoder[CableBoxesBySystem] = deriveDecoder[CableBoxesBySystem]
  implicit val CableBoxesBySystemEncoder: Encoder[CableBoxesBySystem] = deriveEncoder[CableBoxesBySystem]

  implicit val CableRouteDecoder: Decoder[CableRoute] = deriveDecoder[CableRoute]
  implicit val CableRouteEncoder: Encoder[CableRoute] = deriveEncoder[CableRoute]

  implicit val MaterialNodeDecoder: Decoder[MaterialNode] = deriveDecoder[MaterialNode]
  implicit val MaterialNodeEncoder: Encoder[MaterialNode] = deriveEncoder[MaterialNode]

  implicit val EquipmentConnectionDecoder: Decoder[EquipmentConnection] = deriveDecoder[EquipmentConnection]
  implicit val EquipmentConnectionEncoder: Encoder[EquipmentConnection] = deriveEncoder[EquipmentConnection]


  val codecRegistry: CodecRegistry = fromRegistries(fromProviders(

    //MAMONOV
    classOf[CableRoute],
    classOf[EquipmentConnection],

    //BOGDAN

    classOf[PlatePart],
    classOf[ProfilePart],
    classOf[BsDesignNode],
    classOf[HullEsp],
    classOf[PrdPart],
    classOf[HullSystem],
    classOf[PipeSeg],
    classOf[PipeSegActual],
    classOf[Material],
    classOf[ProjectName],
    classOf[SystemDef],
    classOf[PipeSegBilling],
    classOf[SpoolLock],
    classOf[Device],
    classOf[MaterialTranslation],
    classOf[Accommodation],
    classOf[Units],
    classOf[UnitTranslation],
    classOf[BBox],
    classOf[ElecCable],
    classOf[HullEspObject],
    classOf[PipeEspObject],
    classOf[DeviceEspObject],
    classOf[MaterialPurchase],
    classOf[GlobalEsp],
    classOf[DocumentWithMaterial],
    classOf[ElecAngle],
    classOf[MaterialNode],

    //KOKOVIN
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
    classOf[NestLock],
    classOf[MaterialNode],
  ), DEFAULT_CODEC_REGISTRY)

  def mongoDatabase(): MongoDatabase = MongoDB.mongoClient().getDatabase("3degdatabase").withCodecRegistry(codecRegistry)


}
