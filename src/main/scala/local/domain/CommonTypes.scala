package local.domain

object CommonTypes {
  case class ZoneSystem(TYPENAME: String, OID: Int, SEQID: Int, USERID: String, NAME: String, DESCR_RU: String, DESCR_EN: String)

  case class DrawingChessItem(label: String, position: String)

  case class DrawingChess(docNumber: String="",
                          revision: String="",
                          user: String="",
                          date: String="",
                          labels: List[DrawingChessItem]=List.empty[DrawingChessItem]
                         )
  case class Component(oid: Int, stock: String)
  case class SystemLang(system:Int, lang:Int, long_descr:String)
  case class StdProfileMove(oid:Int,  stock:String)
  case class StdPlateMove(oid:Int,  stock:String)


}
