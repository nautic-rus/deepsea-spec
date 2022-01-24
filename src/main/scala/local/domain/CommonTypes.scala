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


}
