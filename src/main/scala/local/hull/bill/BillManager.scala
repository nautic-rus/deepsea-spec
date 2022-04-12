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
                              partsweight: Double = 0.0,
                              isDisabled: Boolean = true
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
                            plateForecast: Int = 0,
                            stock: Int = 0,
                            isDisabled: Boolean = true,
                            oneSheetWeight: Int = 0,
                            wastages: Double = 0.0
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
                            ASPECT_RATIO: Int = 0,
                            STOCK: Int = 0
                          )

  case class ForanMaterial(
                            OID: Int = 0,
                            CODE: String = "",
                            DESCRIPTION: String = "",
                            DENSITY: Double = 0.0
                          )

  case class StdPlate(OID: Int = 0, KPL: Int = 0, MATERIAL_OID: Int = 0, LENGTH: Double = 0.0, WIDTH: Double = 0.0, THICKNESS: Double = 0.0, STORAGE_CODE: String = "", STOCK: Int = 0, area: Double = 0.0)


  case class PlateMaterial(COUNT: Int, WEIGHT: Double, THICKNESS: Double, MAT: String)

  case class ProfileMaterial(KSE: Int = 0, PRF_SECTION: String = "", MATERIAL: String = "", WEB_H: Double = 0.0, WEB_T: Double = 0.0, FLANGE_H: Double = 0.0, FLANGE_T: Double = 0.0, PARTS: Int = 0, LENGHT: Double = 0.0, PARTSWEIGHT: Double = 0.0)

  case class ForanScrap(
                         OID: Int = 0,
                         STDPLATEOID: Int = 0,
                         KPL: Int = 0,
                         PARENTKPL: Int = 0,
                         NESTID: String = "",
                         PARENTNESTID: String = "",
                         TYPE: Int = 0,
                         SLENGTH: Double = 0.0,
                         SWIDTH: Double = 0.0,
                         THICKNESS: Double = 0.0,
                         DENSITY: Double = 0.0,
                         AREAM2: Double = 0.0,
                         WEIGHT: Double = 0.0
                       )


  def genAnalyticProfileData(project: String): List[ProfileAnalitic] = {
    val buff = ListBuffer.empty[ProfileAnalitic]
    val nest: List[ProfileNestBill] = genProfileNestBill(project).sortBy(s => s.KSE)
    val real: List[ProfileMaterial] = genTotProfiles(project).sortBy(s => s.KSE)
    val mats: List[ForanMaterial] = genForanMaterials(project)

    real.foreach(realPart => {
      val realPartsCpunt: Int = realPart.PARTS
      val realLenghtMM: Double = realPart.LENGHT * 1000
      if (nest.exists(s => s.KSE.equals(realPart.KSE))) {
        val nests = nest.filter(p => p.KSE.equals(realPart.KSE) && p.KQ.equals(realPart.MATERIAL))
        val kse = nests.head.KSE
        val mat = nests.head.KQ
        val isDisabled = isMatDisabled(mat, mats)
        val section = nests.head.TP
        val scantling = genScantling(nests.head.WH, nests.head.WT, nests.head.FH, nests.head.FT)
        val grossLenght = nests.head.GROLEN
        val count = nests.map(_.NGB).sum
        val scrap = nests.head.TOTAL_KSE_SCRAP
        val profileForecast: Int = Math.ceil((realLenghtMM + (realLenghtMM / 100) * scrap) / grossLenght).toInt
        buff += ProfileAnalitic(kse, mat, section, scantling, grossLenght, count, scrap, realPartsCpunt, realPart.LENGHT, profileForecast, realPart.PARTSWEIGHT, isDisabled)
      } else {
        val kse = realPart.KSE
        val mat = realPart.MATERIAL
        val isDisabled = isMatDisabled(mat, mats)
        val section = realPart.PRF_SECTION
        val scantling = genScantling(realPart.WEB_H, realPart.WEB_T, realPart.FLANGE_H, realPart.FLANGE_T)
        val grossLenght = 0.0
        val count = 0
        val scrap = 0
        val profileForecast: Int = 0
        buff += ProfileAnalitic(kse, mat, section, scantling, grossLenght, count, scrap, realPartsCpunt, realPart.LENGHT, profileForecast, realPart.PARTSWEIGHT, isDisabled)
      }
    })
    buff.sortBy(s => (s.mat, s.section, s.scantling)).toList
  }

  def genAnalyticPlateData(project: String): List[PlateAnalitic] = {
    val realPrats = genTotPlates(project)
    val nests = genPlateNestBill(project)
    val mats: List[ForanMaterial] = genForanMaterials(project)
    val stdPlates: List[StdPlate] = genForanStdPlates(project)
    val foranScraps: List[ForanScrap] = genPlateForanScrap(project)
    val buff = ListBuffer.empty[PlateAnalitic]

    realPrats.foreach(realPrat => {

      if (nests.exists(p => p.KQ.equals(realPrat.MAT) && p.T == realPrat.THICKNESS)) {
        val nestsByMat: List[PlateNestBill] = nests.filter(p => p.KQ.equals(realPrat.MAT) && p.T == realPrat.THICKNESS)

        val globNest: PlateNestBill = nestsByMat.maxBy(s => s.W * s.L)
        val KPL = globNest.KPL
        val stock = globNest.STOCK
        val mat = realPrat.MAT
        val density: Double = mats.find(s => s.CODE.equals(mat)) match {
          case Some(value) => value.DENSITY
          case None => 0.0d
        }
        val oneSheetWeight = Math.ceil(globNest.W / 1000 * globNest.L / 1000 * (globNest.T / 1000) * density * 1000).toInt
        val isDisabled = isMatDisabled(mat, mats)
        val scantling = genScantling(realPrat.THICKNESS, globNest.L / 1000, globNest.W / 1000)
        val count = nestsByMat.map(_.NGP).sum
        //val scrap = globNest.TOTAL_KPL_SCRAP
        val nestedParts = nestsByMat.map(_.NP).sum
        val realPartsCount = realPrat.COUNT
        val realWeight = realPrat.WEIGHT
        val wastages: Double = calculatePlateWastages(globNest, foranScraps, oneSheetWeight)
        val scrap = calculatePlateScraps(nestsByMat, foranScraps, oneSheetWeight, globNest)

        //val plateForecas = Math.ceil((realWeight + (realWeight / 100) * scrap) / nest.head.TONETWGT).toInt
        //val plateForecas = Math.ceil((realWeight + (realWeight * 0.13 )) / nest.head.TONETWGT).toInt

        val plateForecas = Math.ceil(realWeight / oneSheetWeight + (realWeight / oneSheetWeight) * 0.13d).toInt

        buff += PlateAnalitic(KPL, mat, scantling, count, scrap, nestedParts, realPartsCount, realWeight, plateForecas, stock, isDisabled, oneSheetWeight, wastages)
      } else {
        val mat = realPrat.MAT
        val density: Double = mats.find(s => s.CODE.equals(mat)) match {
          case Some(value) => value.DENSITY
          case None => 0.0d
        }
        val stdPlate = finfSuitableStdPlate(mat, realPrat.THICKNESS, stdPlates, mats)
        val oneSheetWeight = Math.ceil(stdPlate.WIDTH / 1000 * stdPlate.LENGTH / 1000 * (stdPlate.THICKNESS / 1000) * density * 1000).toInt
        val KPL = stdPlate.KPL
        val isDisabled = isMatDisabled(mat, mats)
        val scantling = genScantling(realPrat.THICKNESS, stdPlate.LENGTH / 1000, stdPlate.WIDTH / 1000)
        val count = 0
        val scrap = 0.0
        val realPartsCount = realPrat.COUNT
        val realWeight = realPrat.WEIGHT
        val plateForecas = Math.ceil(realWeight / oneSheetWeight + (realWeight / oneSheetWeight) * 0.13d).toInt
        val nestedPatrs = 0
        val stock = stdPlate.STOCK
        buff += PlateAnalitic(KPL, mat, scantling, count, scrap, nestedPatrs, realPartsCount, realWeight, plateForecas, stock, isDisabled, oneSheetWeight)
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

  private def isMatDisabled(matCode: String, mats: List[ForanMaterial]): Boolean = {
    mats.find(s => s.CODE.equals(matCode)) match {
      case Some(mat) => mat.DESCRIPTION.toUpperCase().contains("DISABLED")
      case None => true
    }
  }

  private def finfSuitableStdPlate(matCode: String, thin: Double, stdPlates: List[StdPlate], mats: List[ForanMaterial]): StdPlate = {
    mats.find(s => s.CODE.equals(matCode)) match {
      case Some(mat) => {
        val stdPlatesMat = stdPlates.filter(s => s.MATERIAL_OID == mat.OID && s.THICKNESS == thin)
        if (stdPlatesMat.nonEmpty) stdPlatesMat.maxBy(s => s.area) else StdPlate()
      }
      case None => StdPlate()
    }
  }

  private def calculatePlateWastages(nest: PlateNestBill, foranScraps: List[ForanScrap], oneSheetWeight: Double): Double = {
    val buff = ListBuffer.empty[ForanScrap]
    val rootKpl = nest.KPL
    foranScraps.filter(s => s.PARENTKPL == rootKpl).foreach(scrL1 => {
      buff += scrL1
      foranScraps.filter(s => s.PARENTKPL == scrL1.KPL).foreach(scrL2 => {
        buff += scrL2
        foranScraps.filter(s => s.PARENTKPL == scrL2.KPL).foreach(scrL3 => {
          buff += scrL3
          foranScraps.filter(s => s.PARENTKPL == scrL3.KPL).foreach(scrL4 => {
            buff += scrL4
            foranScraps.filter(s => s.PARENTKPL == scrL4.KPL).foreach(scrL5 => {
              buff += scrL5
            })
          })
        })
      })

    })
    val totWeught = (buff.filter(s => s.NESTID.isEmpty).map(_.WEIGHT).sum) / oneSheetWeight
    totWeught
  }


  private def calculatePlateScrapsOld(nestsByMat: List[PlateNestBill], foranScraps: List[ForanScrap], oneSheetWeight: Double, globNest: PlateNestBill): Double = {
    var nestCount: Double = 0.0
    var scrapAcc: Double = 0.0
    if (globNest.KPL == 2) {
      val jj = 0
    }

    nestsByMat.foreach(nbm => {
      val realScrap: Double = {
        if (!foranScraps.exists(s => s.KPL.equals(nbm.KPL))) {
          val currscrp = nbm.SCRAP
          val scrps: Double = {
            var scrpW: Double = 0.0
            foranScraps.filter(s => s.PARENTNESTID == nbm.NESTID).foreach(scrL1 => {
              scrpW = scrL1.WEIGHT
              foranScraps.filter(s => s.PARENTNESTID == scrL1.NESTID).foreach(scrL2 => {
                scrpW = scrL2.WEIGHT
                foranScraps.filter(s => s.PARENTNESTID == scrL2.NESTID).foreach(scrL3 => {
                  scrpW = scrL3.WEIGHT
                  foranScraps.filter(s => s.PARENTNESTID == scrL3.NESTID).foreach(scrL4 => {
                    scrpW = scrL4.WEIGHT
                    foranScraps.filter(s => s.PARENTNESTID == scrL4.NESTID).foreach(scrL5 => {
                      scrpW = scrL5.WEIGHT
                    })
                  })
                })
              })
            })
            scrpW
          }
          val realScr = currscrp - (scrps / oneSheetWeight) * 100.0
          nestCount = nestCount + 1.0
          scrapAcc = scrapAcc + realScr
          realScr
        } else {
          nbm.SCRAP
        }
      }

    })
    val ret = scrapAcc / nestCount
    ret
  }


  private def calculatePlateScraps(nestsByMat: List[PlateNestBill], foranScraps: List[ForanScrap], oneSheetWeight: Double, globNest: PlateNestBill): Double = {
    val buff = ListBuffer.empty[Double]
    nestsByMat.filter(s => s.KPL == globNest.KPL).foreach(nbm => {
      var basePartsWeight = nbm.TONETWGT
      foranScraps.filter(s => s.PARENTNESTID == nbm.NESTID).foreach(scrL1 => {
        if (scrL1.NESTID.nonEmpty) {
          nestsByMat.find(s => s.KPL.equals(scrL1.KPL)) match {
            case Some(value) => basePartsWeight = basePartsWeight + value.TONETWGT
            case None => None
          }
        } else {
          basePartsWeight = basePartsWeight + scrL1.WEIGHT
        }

        foranScraps.filter(s => s.PARENTNESTID == scrL1.NESTID).foreach(scrL2 => {
          if (scrL2.NESTID.nonEmpty) {
            nestsByMat.find(s => s.KPL.equals(scrL2.KPL)) match {
              case Some(value) => basePartsWeight = basePartsWeight + value.TONETWGT
              case None => None
            }
          } else {
            basePartsWeight = basePartsWeight + scrL2.WEIGHT
          }

          foranScraps.filter(s => s.PARENTNESTID == scrL2.NESTID).foreach(scrL3 => {
            if (scrL3.NESTID.nonEmpty) {
              nestsByMat.find(s => s.KPL.equals(scrL3.KPL)) match {
                case Some(value) => basePartsWeight = basePartsWeight + value.TONETWGT
                case None => None
              }
            } else {
              basePartsWeight = basePartsWeight + scrL3.WEIGHT
            }

            foranScraps.filter(s => s.PARENTNESTID == scrL3.NESTID).foreach(scrL4 => {
              if (scrL4.NESTID.nonEmpty) {
                nestsByMat.find(s => s.KPL.equals(scrL4.KPL)) match {
                  case Some(value) => basePartsWeight = basePartsWeight + value.TONETWGT
                  case None => None
                }
              } else {
                basePartsWeight = basePartsWeight + scrL4.WEIGHT
              }
              foranScraps.filter(s => s.PARENTNESTID == scrL4.NESTID).foreach(scrL5 => {
                if (scrL5.NESTID.nonEmpty) {
                  nestsByMat.find(s => s.KPL.equals(scrL5.KPL)) match {
                    case Some(value) => basePartsWeight = basePartsWeight + value.TONETWGT
                    case None => None
                  }
                } else {
                  basePartsWeight = basePartsWeight + scrL5.WEIGHT
                }
              })
            })
          })
        })
      })
      buff += (globNest.TOGROWGT - basePartsWeight) / globNest.TOGROWGT * 100
    })
    buff.sum / buff.length
  }
}
