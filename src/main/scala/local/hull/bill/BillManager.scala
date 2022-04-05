package local.hull.bill

import local.common.Codecs
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import scala.collection.mutable.ListBuffer

object BillManager extends BillHelper with Codecs {

  case class ProfileAnalitic(
                              KSE: Int = 0,
                              mat: String = "",
                              section: String = "",
                              scantling: String = "",
                              grossLenght: Double = 0.0,
                              count: Int,
                              scrap: Double = 0.0,
                              realPartsCount: Int = 0,
                              realLenght: Double = 0,
                              profileForecast: Int = 0,
                              partsweight: Double = 0.0
                            )

  case class PlateAnalitic(
                            KPL: Int = 0,
                            mat: String = "",
                            scantling: String = "",
                            count: Int,
                            scrap: Double = 0.0,
                            nestedParts: Int = 0,
                            realPartsCount: Int = 0,
                            realWeight: Double = 0,
                            plateForecast: Int = 0
                          )

  case class ProfileNestBill(
                              KSE: Int = 0,
                              KQ: String = "",
                              TP: String = "",
                              WH: Double = 0.0,
                              WT: Double = 0.0,
                              FH: Double = 0.0,
                              FT: Double = 0.0,
                              NESTID: String = "",
                              NP: Int = 0,
                              NGB: Int = 0,
                              NETLEN: Double = 0.0,
                              GROLEN: Double = 0.0,
                              TONETLEN: Double = 0.0,
                              TOGROLEN: Double = 0.0,
                              TONETWGT: Double = 0.0,
                              TOGROWGT: Double = 0.0,
                              SCRAP: Double = 0.0,
                              BLOCK: String = "",
                              BLOCK_OID: Int = 0,
                              TOTAL_BL_SCRAP: Double = 0.0,
                              TOTAL_KSE_SCRAP: Double = 0.0
                            )

  case class PlateNestBill(
                            KPL: Int = 0,
                            KQ: String = "",
                            L: Double = 0.0,
                            W: Double = 0.0,
                            T: Double = 0.0,
                            NESTID: String = "",
                            NP: Int = 0,
                            NGP: Int = 0,
                            TONETWGT: Double = 0.0,
                            TOGROWGT: Double = 0.0,
                            SCRAP: Double = 0.0,
                            BLOCK: String = "",
                            BLOCK_OID: Int = 0,
                            TOTAL_BL_SCRAP: Double = 0.0,
                            TOTAL_KPL_SCRAP: Double = 0.0,
                            ENCLOS_TYPE: Int = 0,
                            ASPECT_RATIO: Int = 0
                          )

  case class PlateMaterial(COUNT: Int, WEIGHT: Double, THICKNESS: Double, MAT: String)

  case class ProfileMaterial(KSE: Int = 0, PRF_SECTION: String = "", MATERIAL: String = "", WEB_H: Double = 0.0, WEB_T: Double = 0.0, FLANGE_H: Double = 0.0, FLANGE_T: Double = 0.0, PARTS: Int = 0, LENGHT: Double = 0.0, PARTSWEIGHT: Double = 0.0)

  def genAnalyticProfileData(project: String): List[ProfileAnalitic] = {
    val buff = ListBuffer.empty[ProfileAnalitic]
    val nest: List[ProfileNestBill] = genProfileNestBill(project).sortBy(s => s.KSE)
    val real: List[ProfileMaterial] = genTotProfiles(project).sortBy(s => s.KSE)
    real.foreach(realPart => {
      val realPartsCpunt: Int = realPart.PARTS
      val realLenghtMM: Double = realPart.LENGHT * 1000
      if (nest.exists(s => s.KSE.equals(realPart.KSE))) {
        val nests = nest.filter(p => p.KSE.equals(realPart.KSE) && p.KQ.equals(realPart.MATERIAL))
        val kse = nests.head.KSE
        val mat = nests.head.KQ
        val section = nests.head.TP
        val scantling = genScantling(nests.head.WH, nests.head.WT, nests.head.FH, nests.head.FT)
        val grossLenght = nests.head.GROLEN
        val count = nests.map(_.NGB).sum
        val scrap = nests.head.TOTAL_KSE_SCRAP
        val profileForecast: Int = Math.ceil((realLenghtMM + (realLenghtMM / 100) * scrap) / grossLenght).toInt
        buff += ProfileAnalitic(kse, mat, section, scantling, grossLenght, count, scrap, realPartsCpunt, realPart.LENGHT, profileForecast, realPart.PARTSWEIGHT)
      } else {
        val kse = realPart.KSE
        val mat = realPart.MATERIAL
        val section = realPart.PRF_SECTION
        val scantling = genScantling(realPart.WEB_H, realPart.WEB_T, realPart.FLANGE_H, realPart.FLANGE_T)
        val grossLenght = 0.0
        val count = 0
        val scrap = 0
        val profileForecast: Int = 0
        buff += ProfileAnalitic(kse, mat, section, scantling, grossLenght, count, scrap, realPartsCpunt, realPart.LENGHT, profileForecast, realPart.PARTSWEIGHT)
      }
    })
    buff.sortBy(s => (s.mat, s.section, s.scantling)).toList
  }

  def genAnalyticPlateData(project: String): List[PlateAnalitic] = {
    val realPrats = genTotPlates(project)
    val nests = genPlateNestBill(project)

    val buff = ListBuffer.empty[PlateAnalitic]

    realPrats.foreach(realPrat => {

      if (nests.exists(p => p.KQ.equals(realPrat.MAT) && p.T == realPrat.THICKNESS)) {
        val nest = nests.filter(p => p.KQ.equals(realPrat.MAT) && p.T == realPrat.THICKNESS)

        val KPL = nest.head.KPL
        val mat = realPrat.MAT
        val scantling = genScantling(realPrat.THICKNESS, nest.head.L / 1000, nest.head.W / 1000)
        val count = nest.map(_.NGP).sum
        val scrap = nest.head.TOTAL_KPL_SCRAP
        val nestedParts = nest.map(_.NP).sum
        val realPartsCount = realPrat.COUNT
        val realWeight = realPrat.WEIGHT
        val plateForecas = Math.ceil((realWeight + (realWeight / 100) * scrap) / nest.head.TONETWGT).toInt
        buff += PlateAnalitic(KPL, mat, scantling, count, scrap, nestedParts, realPartsCount, realWeight, plateForecas)


      } else {
        val KPL = 0
        val mat = realPrat.MAT
        val scantling = genScantling(realPrat.THICKNESS)
        val count = 0
        val scrap = 0.0
        val realPartsCount = realPrat.COUNT
        val realWeight = realPrat.WEIGHT
        val plateForecas = 0
        val nestedPatrs = 0
        buff += PlateAnalitic(KPL, mat, scantling, count, scrap, nestedPatrs, realPartsCount, realWeight, plateForecas)
      }

    })
    buff.sortBy(s => (s.mat, s.scantling)).toList
  }

  def genAnalyticProfileDataJson(project: String): String = {
    genAnalyticProfileData(project).asJson.noSpaces
  }

  def genAnalyticPlateDataJson(project: String): String = {
    genAnalyticPlateData(project).asJson.noSpaces
  }

}
