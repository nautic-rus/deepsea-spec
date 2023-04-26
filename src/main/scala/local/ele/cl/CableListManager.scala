package local.ele.cl

import local.common.Codecs
import local.ele.CommonEle.Cable
import io.circe.parser._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import local.common.DBRequests.retrieveZoneAndSystems

object CableListManager extends CableListHelper with Codecs {

  def cablesByComplect(project: String, complectName: String): List[Cable] = retrieveCablesByComplect(project, complectName)

  def cablesByComplectJson(project: String, complectName: String): String = cablesByComplect(project, complectName).asJson.noSpaces

  def cablesByComplectMagistralVariant(project: String, complectName: String): List[Cable] = retrieveCablesByComplectMagistralVariant(project, complectName)

  def cablesByComplectMagistralVariantJson(project: String, complectName: String): String = cablesByComplectMagistralVariant(project, complectName).asJson.noSpaces

  def roomsByProject(project: String): String = retrieveZoneAndSystems(project).filter(p => p.TYPENAME.equals("zone")).sortBy(d => d.USERID).asJson.noSpaces

  def cablesByRoom(project: String, room: String): List[Cable] = retrieveCablesByRoom(project, room)

  def cablesByRoomJson(project: String, room: String): String = cablesByRoom(project, room).asJson.noSpaces
}
