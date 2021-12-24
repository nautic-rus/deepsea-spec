package local.ele.eq

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import local.common.DBRequests.MountItem
import local.domain.WorkShopMaterial
import local.ele.CommonEle.EleComplect

object EleEqManager extends EleEqHelper with Codecs {

  case class EleEq(
                    OID: Int = 0,
                    LABEL: String = "",
                    TYPE: Int = 0,
                    USERID: String = "",
                    ZONE_SEQID: Int = 0,
                    ZONE_NAME: String = "",
                    ZONE_DESCR: String = "",
                    SYSTEM_SEQID: Int = 0,
                    SYSTEM_NAME: String = "",
                    SYSTEM_DESCR: String = "",
                    ABBREV: String = "",
                    XCOG: Double = 0.0,
                    YCOG: Double = 0.0,
                    ZCOG: Double = 0.0,
                    WEIGHT: Double = 0.0,
                    STOCK_CODE: String = "",
                    CLASS_NAME: String = "",
                    SURFACE: String = "",
                    MARGIN: Int = 0,
                    SUPPORTS: List[MountItem] = List.empty[MountItem],
                    workShopMaterial: WorkShopMaterial = new WorkShopMaterial()
                  ) {
    def orderItem(): Double = {
      LABEL.toDoubleOption match {
        case Some(value) => value
        case None => 99999999.0
      }
    }
  }


  def genEqLabelsByEqOid(project: String, eqOid: String): List[String] = eqLabelsByEqOid(project, eqOid)

  def genEqLabelsByEqOidJson(project: String, eqOid: String): String = {
    eqLabelsByEqOid(project, eqOid).asJson.noSpaces
  }

  def eqToJson(eq: EleEq): String = eq.asJson.noSpaces


  def genEqByOID(project: String, eqOid: String): EleEq = eqByOID(project, eqOid)

  def genEqByOIDJson(project: String, eqOid: String): String = eqByOID(project, eqOid).asJson.noSpaces

  def genEqsByComplect(project: String, complect: EleComplect): List[EleEq] = eqsByComplect(project, complect)

  def genEqsByComplectJson(project: String, complect: EleComplect): String = genEqsByComplect(project, complect).asJson.noSpaces


}
