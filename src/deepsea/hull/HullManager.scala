package deepsea.hull

import akka.actor.Actor
import deepsea.database.DatabaseManager.GetOracleConnection
import deepsea.hull.HullManager.{GetForanParts, HullPart}
import play.api.libs.json.{Json, OWrites}

import scala.collection.mutable.ListBuffer
import scala.io.Source


object HullManager{
  case class GetForanParts(project: String)
  case class HullPart(PARTOID: Int, SPARTOID: Int, PARTNAME: String, PARTDESCR: String, BLOCKNAME: String, WEIGHT: Double, AREA: Double, THICK: Double, KSE: Int)
  implicit val writesUser: OWrites[HullPart] = Json.writes[HullPart]
}
class HullManager extends Actor{
  override def receive: Receive = {
    case GetForanParts(project) => Json.toJson(getForanParts(project))
  }
  def getForanParts(project: String): ListBuffer[HullPart] ={
    val res = ListBuffer.empty[HullPart]
    GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/hullParts.sql").mkString
        val rs = s.executeQuery(query)
        while (rs.next()){
          res += HullPart(
            rs.getInt("PARTOID"),
            rs.getInt("SPARTOID"),
            rs.getString("PARTNAME"),
            rs.getString("PARTDESCR"),
            rs.getString("BLOCKNAME"),
            rs.getDouble("WEIGHT"),
            rs.getDouble("AREA"),
            rs.getDouble("THICK"),
            rs.getInt("KSE"),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
}
