package local.ele.cbfill.models

object Cases {


  case class ForanCableBox(boxID: String, boxX: Double, boxY: Double, boxZ: Double, roomCode: String, roomName: String, foranName: String, seal: String, cbxType: Int, stockCode: String, foranWeifgt: Double, rooms: String)

  case class ForanCable(cableIndex: String, oDiam: Double)

  case class CableBoxCables(foranCableBox: ForanCableBox, foranCables: List[ForanCable]) {
    override def toString: String = {
      foranCableBox.boxID
    }
  }

  case class CBStuff(cableBoxModules: List[CableBoxModule], sealModules: List[SealModule], compressionBlocks: List[CompressionBlock], ankerPlates: List[AnkerPlate])

  case class CableBoxCable(foranCableBox: ForanCableBox, foranCable: ForanCable)

  case class CablBoxPackItem(foranCable: ForanCable, sealModule: SealModule)

  case class CableBoxPackArea(
                               packsA: List[CablBoxPackItem],
                               packsB: List[CablBoxPackItem],
                               packsC: List[CablBoxPackItem]
                             )

  case class CBXFill(
                      foranCableBox: ForanCableBox,
                      cables: List[ForanCable],
                      cableBoxModule: CableBoxModule,



                    )
}
