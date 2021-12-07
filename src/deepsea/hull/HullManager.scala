package deepsea.hull

import akka.actor.Actor
import deepsea.database.DatabaseManager.GetOracleConnection
import deepsea.hull.HullManager.{GetForanParts, HullPart, HullPartPlateDef, HullPartProfileDef}
import play.api.libs.json.{Json, OWrites}

import scala.collection.mutable.ListBuffer
import scala.io.Source


object HullManager {
  case class GetForanParts(project: String)

  case class HullPart(PARTOID: Int, SPARTOID: Int, PARTNAME: String, PARTDESCR: String, BLOCKNAME: String, WEIGHT: Double, AREA: Double, THICK: Double, KSE: Int, var MATERIAL: String = "", var SECTION: String = "")

  implicit val writesUser: OWrites[HullPart] = Json.writes[HullPart]

  case class HullPartPlateDef(PART_OID: Int, MATERIAL: String, MATERIAL_OID: Int, THICK: Double)

  case class HullPartProfileDef(KSE: Int, SECTION: Int, WEB_HEIGHT: Double, WEB_THICKNESS: Double, FLANGE_HEIGHT: Double, FLANGE_THICKNESS: Double, MATERIAL: String, MATERIAL_OID: Int)
}

class HullManager extends Actor {
  override def receive: Receive = {
    case GetForanParts(project) => Json.toJson(getForanParts(project))
  }

  def getForanParts(project: String): ListBuffer[HullPart] = {
    val res = ListBuffer.empty[HullPart]
    GetOracleConnection(project) match {
      case Some(c) =>
        val s = c.createStatement()
        val query = Source.fromResource("queries/hullParts.sql").mkString
        val rs = s.executeQuery(query)
        while (rs.next()) {
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


    val stdPlates = getStdPlates(project, res.filter(x => x.KSE == 0).map(x => x.PARTOID))
    val stdProfiles = getStdProfiles(project, res.filter(x => x.KSE != 0).map(x => x.KSE))


    res.foreach(part => {
      if (part.KSE != 0) {
        stdProfiles.find(x => x.KSE == part.KSE) match {
          case Some(value) =>
            part.MATERIAL = value.MATERIAL
            part.SECTION = profileSectionDecode(value.SECTION) + List(value.WEB_HEIGHT, value.WEB_THICKNESS, value.FLANGE_HEIGHT, value.FLANGE_THICKNESS).mkString("x")
          case _ => None
        }
      }
      else{
        stdPlates.find(x => x.PART_OID == part.PARTOID) match {
          case Some(value) =>
            part.MATERIAL = value.MATERIAL
            part.SECTION = value.THICK.toString
          case _ => None
        }
      }
    })

    res
  }

  def getStdPlates(project: String, plateOids: ListBuffer[Int]): ListBuffer[HullPartPlateDef] = {
    val res = ListBuffer.empty[HullPartPlateDef]
    plateOids.grouped(900).toList.foreach(oids => {
      GetOracleConnection(project) match {
        case Some(c) =>
          val s = c.createStatement()
          val query = Source.fromResource("queries/hullPartsPlateDefs.sql").mkString.replaceAll("&plateOids", oids.mkString(","))
          val rs = s.executeQuery(query)
          while (rs.next()) {
            res += HullPartPlateDef(
              rs.getInt("PARTOID"),
              rs.getString("MATERIAL"),
              rs.getInt("MATERIAL_OID"),
              rs.getDouble("THICK"),
            )
          }
          rs.close()
          s.close()
          c.close()
        case _ =>
      }
    })

    res
  }

  def getStdProfiles(project: String, kseOids: ListBuffer[Int]): ListBuffer[HullPartProfileDef] = {
    val res = ListBuffer.empty[HullPartProfileDef]
    kseOids.grouped(900).toList.foreach(oids => {
      GetOracleConnection(project) match {
        case Some(c) =>
          val s = c.createStatement()
          val query = Source.fromResource("queries/hullPartsProfileDefs.sql").mkString.replaceAll("&profileKse", oids.mkString(","))
          val rs = s.executeQuery(query)
          while (rs.next()) {
            res += HullPartProfileDef(
              rs.getInt("KSE"),
              rs.getInt("PROF_SECTION"),
              rs.getDouble("WEB_HEIGHT"),
              rs.getDouble("WEB_THICKNESS"),
              rs.getDouble("FLANGE_HEIGHT"),
              rs.getDouble("FLANGE_THICKNESS"),
              rs.getString("MATERIAL"),
              rs.getInt("MATERIAL_OID"),
            )
          }
          rs.close()
          s.close()
          c.close()
        case _ =>
      }
    })

    res
  }

  def profileSectionDecode(kse: Int) = kse match {
    case 0 =>
      "FS"
    case 1 =>
      "AS"
    case 2 =>
      "IS"
    case 3 =>
      "TS"
    case 4 =>
      "US"
    case 5 =>
      "BS"
    case 6 =>
      "ST"
    case 7 =>
      "AT"
    case 8 =>
      "OS"
    case 9 =>
      "PS"
    case 10 =>
      "RS"
    case 11 =>
      "MC"
    case 12 =>
      "DB"
    case 13 =>
      "SR"
    case 14 =>
      "HR"
    case 15 =>
      "LI"
    case 16 =>
      "ZL"
    case 17 =>
      "TL"
    case 18 =>
      "AI"
    case 19 =>
      "BL"
    case 20 =>
      "LA"
    case 21 =>
      "TA"
    case _ =>
      ""
  }
}
