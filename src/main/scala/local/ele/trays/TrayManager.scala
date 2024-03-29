package local.ele.trays

import local.common.Codecs
import local.common.DBRequests.{MountItem, findWorkshopMaterial, retrieveAllMaterialsByProject}
import local.domain.WorkShopMaterial
import local.ele.CommonEle.EleComplect

import scala.collection.mutable.ListBuffer
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps

object TrayManager extends TrayHelper with Codecs {


  case class Tray(foranTray: ForanTray, mountData: TrayMountData, workShopMaterial: WorkShopMaterial, supports: List[MountItem])


  case class TrayMountData(label: String = "NF", trmCode: String = "NF", typeId: Int = 0, typeDescr: String = "NF", matId: Int = 0, matDescr: String = "NF",
                           H: Double = 0, parD1: Double = 0, parD2: Double = 0, parD3: Double = 0, parD4: Double = 0, parD5: Double = 0,
                           parI1: Int = 0, parI2: Int = 0, parS1: String = "", parS2: String = "", parB1: Boolean = true, parB2: Boolean = true)

  case class TrayMountRules(label: String, trmCode: String = "", inputTypeIdRange: String = "", inputTypeDescr: String = "", searchTypeIdRange: String = "", typeDescr: String = "",
                            kei: String = "", count: Double = 0.0, lenghtFactor: Double = 0.0, variant: Int = 1, isNeedLabel: Boolean = false)

  case class ForanTray(
                        IDSQ: Int = 0,
                        FDS_MODEL: Int = 0,
                        ZONE: String = "",
                        SYSTEM: String = "",
                        LINE: Int = 0,
                        PLS: Int = 0,
                        ELEM: Int = 0,
                        WEIGHT: Double = 0,
                        X_COG: Double = 0,
                        Y_COG: Double = 0,
                        Z_COG: Double = 0,
                        CTYPE: String = "",
                        TYPE: Int = 0,
                        NODE1: String = "",
                        NODE2: String = "",
                        TRAY_LEVEL: Int = 0,
                        STOCK_CODE: String = "",
                        N1X: Double = 0,
                        N1Y: Double = 0,
                        N1Z: Double = 0,
                        N2X: Double = 0,
                        N2Y: Double = 0,
                        N2Z: Double = 0,
                        LEN: Double = 0,
                        SURFACE: String = "",
                        trayDescr: String = "",
                        marign: Int = 0,
                        materialId: Int = 0
                      )

  case class ForanCBX(
                       IDSQ: Int = 0,
                       ZONE: String = "",
                       SYSTEM: String = "",
                       X_COG: Double = 0,
                       Y_COG: Double = 0,
                       Z_COG: Double = 0,
                       WEIGHT: Double = 0,
                       NODE1: String = "",
                       NODE2: String = "",
                       CODE: String = "",
                       DESCR: String = "",
                       STOCK_CODE: String = "",
                       PENRTRATION: String = ""
                     )

  def trayLabels(project: String, trayIdSeq: String): List[String] = {
    val ret = ListBuffer.empty[String]
    val traysMountData = retrieveAllTrayMountData()
    val mountData: List[TrayMountData] = retrieveTraysMountDate()
    val mountRules: List[TrayMountRules] = retrieveTraysMountRules()
    val clickTray: ForanTray = TrayBySeqId(project, trayIdSeq)
    val materials = retrieveAllMaterialsByProject(project)
    val tray = calculateTrayMountDate(project, clickTray, mountData, mountRules, materials, traysMountData)
    if (clickTray.marign > 0.0) ret += "V=" + clickTray.marign.toString


    tray.mountData.label match {
      case "NF" => {
        val cbx = retrieveCBXBySQID(project, trayIdSeq)
        val tmd: TrayMountData = traysMountData.find(s => s.trmCode.contains(cbx.STOCK_CODE)).getOrElse(TrayMountData())
        ret += tmd.label
      }
      case _ => {
        ret += tray.mountData.label
        tray.supports.filter(d => d.isNeedLabel).sortBy(d => d.label).foreach(s => ret += s.label)
      }
    }
    ret.distinct.toList
  }

  def tarysByZonesSystems(project: String, zones: List[String], systems: List[String]): List[Tray] = {
    val buff = ListBuffer.empty[Tray]
    val mountData: List[TrayMountData] = retrieveTraysMountDate()
    val mountRules: List[TrayMountRules] = retrieveTraysMountRules()
    val materials: List[WorkShopMaterial] = retrieveAllMaterialsByProject(project)
    val traysMountData: List[TrayMountData] = retrieveAllTrayMountData()
    //.filter(s=>s.IDSQ==18789202)
    retrieveTraysByZoneNameAndSysName(project, zones, systems).foreach(clickTray => {
      buff += calculateTrayMountDate(project, clickTray, mountData, mountRules, materials, traysMountData)
    })

    retrieveCBXByZoneNameAndSysName(project, zones, systems).foreach(cbx => {
      val tmd: TrayMountData = traysMountData.find(s => s.trmCode.contains(cbx.STOCK_CODE)).getOrElse(TrayMountData())
      val ft = ForanTray(IDSQ = cbx.IDSQ, ZONE = cbx.ZONE, SYSTEM = cbx.SYSTEM,
        X_COG = cbx.X_COG, Y_COG = cbx.Y_COG, Z_COG = cbx.Z_COG, WEIGHT = cbx.WEIGHT,
        NODE1 = cbx.NODE1, NODE2 = cbx.NODE2, STOCK_CODE = cbx.STOCK_CODE, LEN = 1000.0)
      val ws = materials.find(s => s.trmCode.contains(cbx.STOCK_CODE)).getOrElse(new WorkShopMaterial())
      if (ws.trmCode.equals("NRF0459FB3AF0BD49")) println("NRF0459FB3AF0BD49")
      val tray = Tray(ft, tmd, ws, List.empty[MountItem])
      buff += tray
    })
    buff.toList
  }

  def traysByComplect(project: String, complect: EleComplect): List[Tray] = {
    tarysByZonesSystems(project, complect.zoneNames, complect.systemNames)
  }

  def tarysByZonesSystemsJson(project: String, zones: List[String], systems: List[String]): String = {
    tarysByZonesSystems(project, zones, systems).asJson.noSpaces
  }

  private def calculateTrayMountDate(project: String, foranTray: ForanTray, mountData: List[TrayMountData], mountRules: List[TrayMountRules], materials: List[WorkShopMaterial], traysMountData: List[TrayMountData]): Tray = {

    def calculateQty(count: Double, lenghtFactor: Double, trayLenght: Double): Double = {
      trayLenght / (count * lenghtFactor)
    }

    val clickTrayMontData: TrayMountData = retrieveTrayMountDataByTrm(foranTray.STOCK_CODE, traysMountData)
    val material = findWorkshopMaterial(foranTray.STOCK_CODE, materials)
    val buffMounts = ListBuffer.empty[MountItem]
    val trayLenght: Double = foranTray.LEN / 1000

    if (foranTray.trayDescr.contains("$K")) {

      if (clickTrayMontData.typeId == 61) {

        val item1 = mountRules.find(s => s.label.equals("4205") && s.inputTypeIdRange.contains("61K;")).getOrElse(TrayMountRules(label = "4205"))
        buffMounts += MountItem(findWorkshopMaterial(item1.trmCode, materials), TrayMountData(item1.label, item1.trmCode).label, item1.kei, calculateQty(item1.count, item1.lenghtFactor, trayLenght), item1.isNeedLabel)

        val item2 = mountRules.find(s => s.label.equals("4206") && s.inputTypeIdRange.contains("61K;")).getOrElse(TrayMountRules(label = "4206"))
        buffMounts += MountItem(findWorkshopMaterial(item2.trmCode, materials), TrayMountData(item2.label, item2.trmCode).label, item2.kei, calculateQty(item2.count, item2.lenghtFactor, trayLenght), item2.isNeedLabel)

        val item3 = mountRules.find(s => s.label.equals("6800") && s.inputTypeIdRange.contains("61K;")).getOrElse(TrayMountRules(label = "6800"))
        buffMounts += MountItem(findWorkshopMaterial(item3.trmCode, materials), TrayMountData(item3.label, item3.trmCode).label, item3.kei, (3 * clickTrayMontData.parD2 / 1000) * (trayLenght / item3.lenghtFactor), item3.isNeedLabel)

        val item4 = mountRules.find(s => s.label.equals("4207") && s.inputTypeIdRange.contains("61K;")).getOrElse(TrayMountRules(label = "4207"))
        buffMounts += MountItem(findWorkshopMaterial(item4.trmCode, materials), TrayMountData(item4.label, item4.trmCode).label, item4.kei, calculateQty(item4.count, item4.lenghtFactor, trayLenght), item4.isNeedLabel)
      }
      if (clickTrayMontData.typeId == 62) {

        val item2 = mountRules.find(s => s.label.equals("4208") && s.inputTypeIdRange.contains("62K;")).getOrElse(TrayMountRules(label = "4208"))
        buffMounts += MountItem(findWorkshopMaterial(item2.trmCode, materials), TrayMountData(item2.label, item2.trmCode).label, item2.kei, calculateQty(item2.count, item2.lenghtFactor, trayLenght), item2.isNeedLabel)

        val item3 = mountRules.find(s => s.label.equals("4209") && s.inputTypeIdRange.contains("62K;")).getOrElse(TrayMountRules(label = "4209"))
        buffMounts += MountItem(findWorkshopMaterial(item3.trmCode, materials), TrayMountData(item3.label, item3.trmCode).label, item3.kei, calculateQty(item3.count, item3.lenghtFactor, trayLenght), item3.isNeedLabel)

        val item4 = mountRules.find(s => s.label.equals("4210") && s.inputTypeIdRange.contains("62K;")).getOrElse(TrayMountRules(label = "4210"))
        buffMounts += MountItem(findWorkshopMaterial(item4.trmCode, materials), TrayMountData(item4.label, item4.trmCode).label, item4.kei, calculateQty(item4.count, item4.lenghtFactor, trayLenght), item4.isNeedLabel)


        val item5 = mountData.find(s => s.typeId == 65 && s.matId == clickTrayMontData.matId && s.parI1 == clickTrayMontData.parD2.toInt).getOrElse(TrayMountData(label = "NF"))
        buffMounts += MountItem(findWorkshopMaterial(item5.trmCode, materials), TrayMountData(item5.label, item5.trmCode).label, "006", trayLenght, true)

        val item6 = mountData.find(s => s.typeId == 66 && s.matId == clickTrayMontData.matId).getOrElse(TrayMountData(label = "NF"))
        buffMounts += MountItem(findWorkshopMaterial(item6.trmCode, materials), TrayMountData(item6.label, item6.trmCode).label, "796", trayLenght / 0.75d, false)
      }

    }

    if (clickTrayMontData.typeId == 63) {
      if (!foranTray.SURFACE.contains("F0")) {

        /*      val item3 = mountRules.find(s => s.label.equals("4209") && s.inputTypeIdRange.contains("63;")).getOrElse(TrayMountRules(label = "4209"))
                buffMounts += MountItem(findWorkshopMaterial(item3.trmCode, materials), TrayMountData(item3.label, item3.trmCode).label, item3.kei, trayLenght / 0.5, item3.isNeedLabel)

                val item4 = mountRules.find(s => s.label.equals("4210") && s.inputTypeIdRange.contains("63;")).getOrElse(TrayMountRules(label = "4210"))
                buffMounts += MountItem(findWorkshopMaterial(item4.trmCode, materials), TrayMountData(item4.label, item4.trmCode).label, item4.kei, trayLenght / 0.5, item4.isNeedLabel)*/


        //val item6 = mountData.find(s => s.typeId == 50 && s.matId == foranTray.materialId).getOrElse(TrayMountData(label = "NF"))
        // buffMounts += MountItem(findWorkshopMaterial(item6.trmCode, materials), TrayMountData(item6.label, item6.trmCode).label, "796", 2 * trayLenght, true)

        val it = mountData.filter(s => (s.typeId == 50) && s.matId == clickTrayMontData.matId && s.parD1 * 1000 >= foranTray.marign)
        if (it.nonEmpty) {
          val part = it.minBy(d => d.parD1)
          buffMounts += MountItem(findWorkshopMaterial(part.trmCode, materials), TrayMountData(part.label, part.trmCode).label, "796", 2 * trayLenght, true)
        } else {
          MountItem()
        }


        val item7 = mountData.find(s => s.label.equals("4022")).getOrElse(TrayMountData(label = "4022"))
        buffMounts += MountItem(findWorkshopMaterial(item7.trmCode, materials), TrayMountData(item7.label, item7.trmCode).label, "796", trayLenght / 0.5, false)

        val item8 = mountData.find(s => s.label.equals("4023")).getOrElse(TrayMountData(label = "4023"))
        buffMounts += MountItem(findWorkshopMaterial(item8.trmCode, materials), TrayMountData(item8.label, item8.trmCode).label, "796", trayLenght / 0.5, false)


      } else {
        val item4 = mountData.find(s => s.label.equals("4211")).getOrElse(TrayMountData(label = "4211"))
        buffMounts += MountItem(findWorkshopMaterial(item4.trmCode, materials), TrayMountData(item4.label, item4.trmCode).label, "796", trayLenght / 0.25, false)
      }
    }


    if (clickTrayMontData.typeId == 75) {
      if (!foranTray.SURFACE.contains("F0")) {
        val it = mountData.filter(s => (s.typeId == 50) && s.matId == clickTrayMontData.matId && s.parD1 * 1000 >= foranTray.marign)
        if (it.nonEmpty) {
          val part = it.minBy(d => d.parD1)
          buffMounts += MountItem(findWorkshopMaterial(part.trmCode, materials), TrayMountData(part.label, part.trmCode).label, "796", 2 * trayLenght, true)
        } else {
          MountItem()
        }
        val item5 = mountData.find(s => s.label.equals("4026")).getOrElse(TrayMountData(label = "4026"))
        buffMounts += MountItem(findWorkshopMaterial(item5.trmCode, materials), TrayMountData(item5.label, item5.trmCode).label, "796", trayLenght / 0.5, false)


        val item6 = mountData.find(s => s.label.equals("7700")).getOrElse(TrayMountData(label = "7700"))
        buffMounts += MountItem(findWorkshopMaterial(item6.trmCode, materials), TrayMountData(item6.label, item6.trmCode).label, "166", trayLenght / 10.0, false)

        val item7 = mountData.find(s => s.label.equals("4024")).getOrElse(TrayMountData(label = "4024"))
        buffMounts += MountItem(findWorkshopMaterial(item7.trmCode, materials), TrayMountData(item7.label, item7.trmCode).label, "006", trayLenght / 20.0, false)

        val item8 = mountData.find(s => s.label.equals("4025")).getOrElse(TrayMountData(label = "4025"))
        buffMounts += MountItem(findWorkshopMaterial(item8.trmCode, materials), TrayMountData(item8.label, item8.trmCode).label, "796", trayLenght / 1.0, false)
      }
      else
      {
        val item4 = mountData.find(s => s.label.equals("4024")).getOrElse(TrayMountData(label = "4024"))
        buffMounts += MountItem(findWorkshopMaterial(item4.trmCode, materials), TrayMountData(item4.label, item4.trmCode).label, "006", trayLenght / 5.0, false)

        val item5 = mountData.find(s => s.label.equals("4025")).getOrElse(TrayMountData(label = "4025"))
        buffMounts += MountItem(findWorkshopMaterial(item5.trmCode, materials), TrayMountData(item5.label, item5.trmCode).label, "796", trayLenght / 0.5, false)

      }
    }


    mountRules.filter(p => p.inputTypeIdRange.contains(clickTrayMontData.typeId.toString + ";")).foreach(item => {

      if (item.trmCode.nonEmpty) {
        item.label match {
          case "4002" => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(item.label, item.trmCode).label, item.kei, (3 * clickTrayMontData.parD2 / 1000) * (trayLenght / item.lenghtFactor), item.isNeedLabel)
          //case _ => None
          case _ => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(item.label, item.trmCode).label, item.kei, calculateQty(item.count, item.lenghtFactor, trayLenght), item.isNeedLabel)
        }

      } else {
        item.searchTypeIdRange match {
          case "51;" =>
            mountData.find(s => s.typeId == 51 && s.matId == clickTrayMontData.matId) match {
              case Some(value) => buffMounts += MountItem(findWorkshopMaterial(value.trmCode, materials), TrayMountData(value.label, value.trmCode).label, item.kei, calculateQty(item.count, item.lenghtFactor, trayLenght), item.isNeedLabel)
              case None => MountItem()
            }

          case "65;" =>
            mountData.find(s => s.typeId == 65 && s.matId == clickTrayMontData.matId && s.parI1 == clickTrayMontData.parD2.toInt) match {
              case Some(value) => buffMounts += MountItem(findWorkshopMaterial(value.trmCode, materials), TrayMountData(value.label, value.trmCode).label, item.kei, calculateQty(item.count, item.lenghtFactor, trayLenght), item.isNeedLabel)
              case None => MountItem()
            }
          case "66;" =>
            mountData.find(s => s.typeId == 66 && s.matId == clickTrayMontData.matId) match {
              case Some(value) => buffMounts += MountItem(findWorkshopMaterial(value.trmCode, materials), TrayMountData(value.label, value.trmCode).label, item.kei, calculateQty(item.count, item.lenghtFactor, trayLenght), item.isNeedLabel)
              case None => MountItem()
            }
          case "57;" =>
            val it = mountData.filter(s => (s.typeId == 57) && s.matId == clickTrayMontData.matId && s.parD1 * 1000 >= foranTray.marign)
            if (it.nonEmpty) {
              val part = it.minBy(d => d.parD1)
              buffMounts += MountItem(findWorkshopMaterial(part.trmCode, materials), TrayMountData(part.label, part.trmCode).label, item.kei, calculateQty(item.count, item.lenghtFactor, trayLenght), item.isNeedLabel)
            } else {
              MountItem()
            }
          case "72;" =>
            val it = mountData.filter(s => (s.typeId == 72) && s.parD2 >= clickTrayMontData.parD2)
            if (it.nonEmpty) {
              val part: TrayMountData = it.minBy(d => d.parD2)
              buffMounts += MountItem(findWorkshopMaterial(part.trmCode, materials), TrayMountData(part.label, part.trmCode).label, item.kei, calculateQty(item.count, item.lenghtFactor, trayLenght), item.isNeedLabel)
            } else {
              MountItem()
            }

          case "76;" =>
            val it = mountData.filter(s => (s.typeId == 76) && s.parD2 == clickTrayMontData.parD2)
            if (it.nonEmpty) {
              val part: TrayMountData = it.head
              buffMounts += MountItem(findWorkshopMaterial(part.trmCode, materials), TrayMountData(part.label, part.trmCode).label, item.kei, calculateQty(item.count, item.lenghtFactor, trayLenght), item.isNeedLabel)
            } else {
              MountItem()
            }

          case _ => None
        }
      }
    })


    Tray(foranTray, clickTrayMontData, material, buffMounts.toList.sortBy(s => s.label))
  }

  def postProcessSupports(supports: List[MountItem], project: String): List[MountItem] = {
    val lb = ListBuffer.empty[MountItem]
    val mountData: List[TrayMountData] = retrieveTraysMountDate()
    val materials = retrieveAllMaterialsByProject(project)

    supports.filter(s => s.label.startsWith("57")).groupBy(d => d.label).foreach(item => {
      val item57Count: Int = Math.ceil(item._2.map(_.qty).sum).toInt

      val addData = if (item57Count < 4 && item57Count > 0) 4 - item57Count else 0


      if (addData != 0 && item._2.nonEmpty) lb += MountItem(item._2.head.workShopMaterial, item._2.head.label, item._2.head.kei, addData, item._2.head.isNeedLabel)


      val item3 = mountData.find(s => s.label.equals("4204")).getOrElse(TrayMountData(label = "4204"))
      lb += MountItem(findWorkshopMaterial(item3.trmCode, materials), TrayMountData(item3.label, item3.trmCode).label, "796", item57Count + addData, false)

      val item2 = mountData.find(s => s.label.equals("4203")).getOrElse(TrayMountData(label = "4203"))
      lb += MountItem(findWorkshopMaterial(item2.trmCode, materials), TrayMountData(item2.label, item2.trmCode).label, "796", item57Count + addData, false)

      val item1 = mountData.find(s => s.label.equals("4202")).getOrElse(TrayMountData(label = "4202"))
      lb += MountItem(findWorkshopMaterial(item1.trmCode, materials), TrayMountData(item1.label, item1.trmCode).label, "796", item57Count + addData, false)

    })

    supports.filter(s => s.label.startsWith("76")).groupBy(d => d.label).foreach(item => {
      val item76Count: Int = Math.ceil(item._2.map(_.qty).sum).toInt
      val addData = if (item76Count < 4 && item76Count > 0) 2 - item76Count else 0
      if (addData != 0 && item._2.nonEmpty) lb += MountItem(item._2.head.workShopMaterial, item._2.head.label, item._2.head.kei, addData, item._2.head.isNeedLabel)
    })

    supports.filter(s => s.label.startsWith("41")).groupBy(d => d.label).foreach(item => {
      val item41Count: Int = Math.ceil(item._2.map(_.qty).sum).toInt
      val item3 = mountData.find(s => s.label.equals("4208")).getOrElse(TrayMountData(label = "4208"))
      lb += MountItem(findWorkshopMaterial(item3.trmCode, materials), TrayMountData(item3.label, item3.trmCode).label, "796", item41Count * 2, false)
      val item2 = mountData.find(s => s.label.equals("4209")).getOrElse(TrayMountData(label = "4209"))
      lb += MountItem(findWorkshopMaterial(item2.trmCode, materials), TrayMountData(item2.label, item2.trmCode).label, "796", item41Count * 4, false)
      val item1 = mountData.find(s => s.label.equals("4210")).getOrElse(TrayMountData(label = "4210"))
      lb += MountItem(findWorkshopMaterial(item1.trmCode, materials), TrayMountData(item1.label, item1.trmCode).label, "796", item41Count * 2, false)
    })

    supports.filter(s => s.label.startsWith("72")).groupBy(d => d.label).foreach(item => {
      val item72Count: Int = Math.ceil(item._2.map(_.qty).sum).toInt
      val addData = if (item72Count < 2 && item72Count > 0) 2 - item72Count else 0
      if (addData != 0 && item._2.nonEmpty) lb += MountItem(item._2.head.workShopMaterial, item._2.head.label, item._2.head.kei, addData, item._2.head.isNeedLabel)
    })

    supports.filter(s => s.label.startsWith("66")).groupBy(d => d.label).foreach(item => {
      val item66Count: Int = Math.ceil(item._2.map(_.qty).sum).toInt
      val addData: Int = {
        if (item66Count < 4 && item66Count > 0) 4 - item66Count
        else if (item66Count % 2 != 0) 1 else 0
      }
      if (addData != 0 && item._2.nonEmpty) lb += MountItem(item._2.head.workShopMaterial, item._2.head.label, item._2.head.kei, addData, item._2.head.isNeedLabel)
    })


    lb ++= supports
    lb.toList
  }

  def genCablesByTraySeqId(project: String, trayIdSeq: String): List[String] = cablesByTraySeqId(project, trayIdSeq)

  def genCablesByTraySeqIdAndComplect(project: String, trayIdSeq: String, complect: String): List[String] = cablesByTraySeqIdAndComplect(project, trayIdSeq, complect)

  def genTraysByZoneNameAndSysName(project: String = "P701", zones: List[String] = List.empty[String], systems: List[String] = List.empty[String]): List[ForanTray] = retrieveTraysByZoneNameAndSysName(project, zones, systems)

  def genCablesInLineByTwoNodes(project: String, nodeName1: String, nodeName2: String): List[String] = cablesinLineByTwoNodeNames(project, nodeName1, nodeName2)

  def genCablesInLineByTwoNodesAndComplect(project: String, nodeName1: String, nodeName2: String, complect: String): List[String] = cablesinLineByTwoNodeNamesAndComplect(project, nodeName1, nodeName2, complect)


  def genTraysListByZoneNameAndSysName(project: String = "P701", zones: List[String] = List.empty[String], systems: List[String] = List.empty[String]): List[Tray] = {
    val buff = ListBuffer.empty[Tray]

    buff.toList
  }


}
