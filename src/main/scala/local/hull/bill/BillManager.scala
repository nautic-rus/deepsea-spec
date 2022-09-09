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
                              grossWeight: Double = 0.0,
                              count: Int,
                              scrap: Double = 0.0,
                              realPartsCount: Int = 0,
                              realLenght: Double = 0,
                              profileForecast: Int = 0,
                              partsweight: Double = 0.0,
                              isDisabled: Boolean = true,
                              stock: Int = 0,
                              stockCode: String = ""
                            )

  case class PlateAnalitic(
                            KPL: Int = 0,
                            mat: String = "",
                            scantling: String = "",
                            count: Int,
                            scrap: Double = 0.0,
                            nestedParts: Int = 0,
                            nestedPartsWeight: Double = 0,
                            realPartsCount: Int = 0,
                            realWeight: Double = 0,
                            plateForecast: Int = 0,
                            stock: Int = 0,
                            isDisabled: Boolean = true,
                            oneSheetWeight: Int = 0,
                            wastages: Double = 0.0,
                            wastagesTotal: List[ForanScrap] = List.empty[ForanScrap],
                            stockCode: String = ""
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
                              TOTAL_KSE_SCRAP: Double = 0.0,
                              STOCK: Int = 0,
                              STOCK_CODE: String = ""
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

  case class ProfileMaterial(
                              KSE: Int = 0,
                              PRF_SECTION: String = "",
                              MATERIAL: String = "",
                              WEB_H: Double = 0.0,
                              WEB_T: Double = 0.0,
                              FLANGE_H: Double = 0.0,
                              FLANGE_T: Double = 0.0,
                              PARTS: Int = 0,
                              LENGHT: Double = 0.0,
                              PARTSWEIGHT: Double = 0.0,
                              STOCK: Int = 0,
                              GROWLEN: Double = 0.0,
                              GROWWEIGHT: Double = 0.0
                            )

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
                         WEIGHT: Double = 0.0,
                       )


  def genProfileNestBillJson(project: String):String= genProfileNestBill(project).sortBy(s => s.KSE).asJson.noSpaces




  def genAnalyticProfileData(project: String): List[ProfileAnalitic] = {
    val buff = ListBuffer.empty[ProfileAnalitic]
    val nest: List[ProfileNestBill] = genProfileNestBill(project).sortBy(s => s.KSE)
    val real: List[ProfileMaterial] = genTotProfiles(project).sortBy(s => s.KSE)
    val mats: List[ForanMaterial] = genForanMaterials(project)

    real.foreach(realPart => {
      val realPartsCpunt: Int = realPart.PARTS
      //val realLenghtMM: Double = realPart.LENGHT * 1000
      if (nest.exists(s => s.KSE.equals(realPart.KSE))) {
        val nests = nest.filter(p => p.KSE.equals(realPart.KSE) && p.KQ.equals(realPart.MATERIAL))
        val nestGRO: ProfileNestBill = nests.maxBy(s => s.GROLEN)
        val kse = nestGRO.KSE
        val mat = nestGRO.KQ
        val stock = nestGRO.STOCK

        //modification by Bogdan
        val stockCode = nest.find(_.KSE == realPart.KSE) match {
          case Some(value) => value.STOCK_CODE
          case _ => ""
        }
        //modification by Bogdan


        val isDisabled = isMatDisabled(mat, mats)
        val section = nests.head.TP
        val scantling = genScantling(nestGRO.WH, nestGRO.WT, nestGRO.FH, nestGRO.FT)
        val grossLenght = nestGRO.GROLEN
        val count = nests.map(_.NGB).sum
        val scrap = nestGRO.TOTAL_KSE_SCRAP

        //val profileForecast: Int = Math.ceil((realLenghtMM + (realLenghtMM / 100) * scrap) / grossLenght).toInt
        val profileForecast: Int = {
          val res = count + Math.ceil(((realPart.LENGHT * 1000 - (count * grossLenght)) / grossLenght) * 1.11d).toInt
          if (res <= 0) count else res
        }


        buff += ProfileAnalitic(kse, mat, section, scantling, grossLenght, realPart.GROWWEIGHT, count, scrap, realPartsCpunt, realPart.LENGHT, profileForecast, realPart.PARTSWEIGHT, isDisabled, stock, stockCode)
      } else {
        val kse = realPart.KSE
        val mat = realPart.MATERIAL
        val isDisabled = isMatDisabled(mat, mats)
        val section = realPart.PRF_SECTION
        val scantling = genScantling(realPart.WEB_H, realPart.WEB_T, realPart.FLANGE_H, realPart.FLANGE_T)
        val stock = realPart.STOCK
        val grossLenght = realPart.GROWLEN
        val count = 0
        val scrap = 0

        //modification by Bogdan
        val stockCode = nest.find(_.KSE == realPart.KSE) match {
          case Some(value) => value.STOCK_CODE
          case _ => ""
        }
        //modification by Bogdan


        val profileForecast: Int = {

          if (realPart.GROWLEN <= 0.0d) {
            0
          } else {
            Math.ceil((realPart.LENGHT / realPart.GROWLEN) * 1.11d).toInt
          }
        }

        buff += ProfileAnalitic(kse, mat, section, scantling, grossLenght * 1000, realPart.GROWWEIGHT, count, scrap, realPartsCpunt, realPart.LENGHT, profileForecast, realPart.PARTSWEIGHT, isDisabled, stock, stockCode)
      }
    })
    buff.sortBy(s => (s.mat, s.section, s.scantling)).toList
  }

  def genAnalyticPlateData(project: String): List[PlateAnalitic] = {
    val realPrats = genTotPlates(project)
    val nests = genPlateNestBill(project)
    val mats: List[ForanMaterial] = genForanMaterials(project)
    val stdPlates: List[StdPlate] = genForanStdPlates(project)

    val stdPlatesInStocks: List[StdPlate] = genForanStdPlates(project).filter(s=>s.STOCK>0)
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
        val nestedPartsWeight = nestsByMat.map(_.TONETWGT).sum
        val isDisabled = isMatDisabled(mat, mats)
        val scantling = genScantling(realPrat.THICKNESS, globNest.L / 1000, globNest.W / 1000)
        val count = calculateNestPlatesCount(nestsByMat, foranScraps)
        val nestedParts = nestsByMat.map(_.NP).sum
        val realPartsCount = realPrat.COUNT
        val realWeight = realPrat.WEIGHT
        val wastagesToatal: List[ForanScrap] = genWastsgeByParentKpl(project, KPL)
        val wastagesWeight: Double = wastagesToatal.map(_.WEIGHT).sum
        val scrap = calculatePlateScraps(nestsByMat, foranScraps, oneSheetWeight, globNest)

        //modification by Bogdan
        val stdPlate = findSuitableStdPlate(mat, realPrat.THICKNESS, stdPlates, mats)
        val stockCode = stdPlate.STORAGE_CODE
        //modification by Bogdan


        val plateForecas: Int = {
          val partsToNestWeight: Double = {
            val v: Double = realPrat.WEIGHT - nestedPartsWeight
            if (v <= 0.0d) 0.0d else v
          }
          val alreadyUsedGrossPlatesWeight = (oneSheetWeight * count) - wastagesWeight
          val ret = Math.ceil(((alreadyUsedGrossPlatesWeight + partsToNestWeight * 1.13d) / oneSheetWeight)).toInt
          ret
        }
        val wastages = wastagesWeight / oneSheetWeight
        buff += PlateAnalitic(KPL, mat, scantling, count, scrap, nestedParts, nestedPartsWeight, realPartsCount, realWeight, plateForecas, stock, isDisabled, oneSheetWeight, wastages, wastagesToatal, stockCode)

      } else {
        val mat = realPrat.MAT
        val density: Double = mats.find(s => s.CODE.equals(mat)) match {
          case Some(value) => value.DENSITY
          case None => 0.0d
        }
        val stdPlate = findSuitableStdPlate(mat, realPrat.THICKNESS, stdPlates, mats)
        val oneSheetWeight = Math.ceil(stdPlate.WIDTH / 1000 * stdPlate.LENGTH / 1000 * (stdPlate.THICKNESS / 1000) * density * 1000).toInt
        val KPL = stdPlate.KPL
        val isDisabled = isMatDisabled(mat, mats)
        val scantling: String = genScantling(realPrat.THICKNESS, stdPlate.LENGTH / 1000, stdPlate.WIDTH / 1000)
        val count = 0
        val scrap = 0.0
        val realPartsCount = realPrat.COUNT
        val realWeight = realPrat.WEIGHT
        val plateForecas = if (oneSheetWeight == 0.0) 0 else Math.ceil(((realWeight / oneSheetWeight)) * 1.13d).toInt
        val nestedPatrs = 0
        val nestedPartsWeight: Double = 0.0
        val stock = stdPlate.STOCK

        //modification by Bogdan
        val stockCode = stdPlate.STORAGE_CODE
        //modification by Bogdan

        buff += PlateAnalitic(KPL, mat, scantling, count, scrap, nestedPatrs, nestedPartsWeight, realPartsCount, realWeight, plateForecas, stock, isDisabled, oneSheetWeight, 0, List.empty[ForanScrap], stockCode)
      }

    })

    stdPlatesInStocks.foreach(stdP=>{
      buff.find(s => s.KPL == stdP.KPL) match {
        case Some(value) => None
        case None => {
          mats.find(m => m.OID == stdP.MATERIAL_OID) match {
            case Some(mat) =>{
              val isDisabled = isMatDisabled(mat.CODE, mats)
              val scantling: String = genScantling(stdP.THICKNESS, stdP.LENGTH / 1000, stdP.WIDTH / 1000)
              val oneSheetWeight = Math.ceil(stdP.WIDTH / 1000 * stdP.LENGTH / 1000 * (stdP.THICKNESS / 1000) * mat.DENSITY * 1000).toInt
              buff+=PlateAnalitic(stdP.KPL, mat.CODE, scantling, 0, 0.0, 0, 0.0, 0, 0.0, 0, stdP.STOCK, isDisabled, oneSheetWeight, 0.0, List.empty[ForanScrap], stdP.STORAGE_CODE)
            }
            case None => None
          }
        }
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

  def genWastsgeByParentKpl(project: String, KPL: Int): List[ForanScrap] = {
    val buff = ListBuffer.empty[ForanScrap]
    val foranScraps: List[ForanScrap] = genPlateForanScrap(project)
    foranScraps.filter(s => s.PARENTKPL == KPL).foreach(scrL1 => {
      if (scrL1.NESTID.isEmpty) buff += scrL1
      foranScraps.filter(s => s.PARENTNESTID == scrL1.NESTID).foreach(scrL2 => {
        if (scrL2.NESTID.isEmpty) buff += scrL2
        foranScraps.filter(s => s.PARENTNESTID == scrL2.NESTID).foreach(scrL3 => {
          if (scrL3.NESTID.isEmpty) buff += scrL3
          foranScraps.filter(s => s.PARENTNESTID == scrL3.NESTID).foreach(scrL4 => {
            if (scrL4.NESTID.isEmpty) buff += scrL4
            foranScraps.filter(s => s.PARENTNESTID == scrL4.NESTID).foreach(scrL5 => {
              if (scrL5.NESTID.isEmpty) buff += scrL5
            })
          })
        })
      })
    })
    buff.toList
  }

  def genWastsgeByParentKplJson(project: String, KPL: Int): String = genWastsgeByParentKpl(project, KPL).asJson.noSpaces

  private def isMatDisabled(matCode: String, mats: List[ForanMaterial]): Boolean = {
    mats.find(s => s.CODE.equals(matCode)) match {
      case Some(mat) => mat.DESCRIPTION.toUpperCase().contains("DISABLED")
      case None => true
    }
  }

  private def findSuitableStdPlate(matCode: String, thin: Double, stdPlates: List[StdPlate], mats: List[ForanMaterial]): StdPlate = {
    mats.find(s => s.CODE.equals(matCode)) match {
      case Some(mat) => {
        val stdPlatesMat = stdPlates.filter(s => s.MATERIAL_OID == mat.OID && s.THICKNESS == thin)
        if (stdPlatesMat.nonEmpty) stdPlatesMat.maxBy(s => s.area) else StdPlate()
      }
      case None => StdPlate()
    }
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

  private def calculateNestPlatesCount(nestsByMat: List[PlateNestBill], foranScraps: List[ForanScrap]): Int = {
    val buff = ListBuffer.empty[PlateNestBill]
    nestsByMat.foreach(s => {
      if (!foranScraps.exists(d => d.KPL.equals(s.KPL))) buff += s
    })
    buff.map(_.NGP).sum
  }
}
