package local.ele.trays

import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import local.common.Codecs
import local.common.DBRequests._
import local.domain.WorkShopMaterial
import local.ele.CommonEle.EleComplect

import scala.collection.mutable.ListBuffer

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
    val trayLenght: Double = foranTray.LEN /1000

    if (foranTray.trayDescr.contains("$K")) {

      if (clickTrayMontData.typeId == 61) {

        val item1 = mountRules.find(s => s.label.equals("4205") && s.inputTypeIdRange.equals("61;")).getOrElse(TrayMountRules(label = "4205"))
        buffMounts += MountItem(findWorkshopMaterial(item1.trmCode, materials), TrayMountData(item1.label, item1.trmCode).label, item1.kei,calculateQty(item1.count, item1.lenghtFactor,  trayLenght), item1.isNeedLabel)

        val item2 = mountRules.find(s => s.label.equals("4206") && s.inputTypeIdRange.equals("61;")).getOrElse(TrayMountRules(label = "4206"))
        buffMounts += MountItem(findWorkshopMaterial(item2.trmCode, materials), TrayMountData(item2.label, item2.trmCode).label, item2.kei, calculateQty(item2.count, item2.lenghtFactor,  trayLenght), item2.isNeedLabel)

        val item3 = mountRules.find(s => s.label.equals("4089") && s.inputTypeIdRange.equals("61;")).getOrElse(TrayMountRules(label = "4089"))
        buffMounts += MountItem(findWorkshopMaterial(item3.trmCode, materials), TrayMountData(item3.label, item3.trmCode).label, item3.kei,  calculateQty(item3.count , item3.lenghtFactor , trayLenght), item3.isNeedLabel)

        val item4 = mountRules.find(s => s.label.equals("4207") && s.inputTypeIdRange.equals("61;")).getOrElse(TrayMountRules(label = "4207"))
        buffMounts += MountItem(findWorkshopMaterial(item4.trmCode, materials), TrayMountData(item4.label, item4.trmCode).label, item4.kei,  calculateQty(item4.count , item4.lenghtFactor , trayLenght), item4.isNeedLabel)
      }
      if (clickTrayMontData.typeId == 62) {
        val item2 = mountRules.find(s => s.label.equals("4208") && s.inputTypeIdRange.equals("62;")).getOrElse(TrayMountRules(label = "4208"))
        buffMounts += MountItem(findWorkshopMaterial(item2.trmCode, materials), TrayMountData(item2.label, item2.trmCode).label, item2.kei,  calculateQty(item2.count , item2.lenghtFactor , trayLenght), item2.isNeedLabel)

        val item3 = mountRules.find(s => s.label.equals("4209") && s.inputTypeIdRange.equals("62;")).getOrElse(TrayMountRules(label = "4209"))
        buffMounts += MountItem(findWorkshopMaterial(item3.trmCode, materials), TrayMountData(item3.label, item3.trmCode).label, item3.kei, calculateQty( item3.count , item3.lenghtFactor , trayLenght), item3.isNeedLabel)

        val item4 = mountRules.find(s => s.label.equals("4210") && s.inputTypeIdRange.equals("62;")).getOrElse(TrayMountRules(label = "4210"))
        buffMounts += MountItem(findWorkshopMaterial(item4.trmCode, materials), TrayMountData(item4.label, item4.trmCode).label, item4.kei, calculateQty(item4.count , item4.lenghtFactor , trayLenght), item4.isNeedLabel)

        val item5 = mountData.find(s => s.typeId == 65 && s.matId == foranTray.materialId && s.parI1 == clickTrayMontData.parD2.toInt).getOrElse(TrayMountData(label = "NF"))
        buffMounts += MountItem(findWorkshopMaterial(item5.trmCode, materials), TrayMountData(item5.label, item5.trmCode).label, "006", trayLenght, true)

        val item6 = mountData.find(s => s.typeId == 66 && s.matId == foranTray.materialId).getOrElse(TrayMountData(label = "NF"))
        buffMounts += MountItem(findWorkshopMaterial(item6.trmCode, materials), TrayMountData(item6.label, item6.trmCode).label, "796",  trayLenght/0.3, true)
      }

    }

    if (clickTrayMontData.typeId == 63) {
      if (foranTray.SURFACE.contains("F0")) {

        val item2 = mountRules.find(s => s.label.equals("4022") && s.inputTypeIdRange.equals("63;")).getOrElse(TrayMountRules(label = "4022"))
        buffMounts += MountItem(findWorkshopMaterial(item2.trmCode, materials), TrayMountData(item2.label, item2.trmCode).label, item2.kei, item2.count * item2.lenghtFactor * trayLenght, item2.isNeedLabel)

        val item3 = mountRules.find(s => s.label.equals("4209") && s.inputTypeIdRange.equals("63;")).getOrElse(TrayMountRules(label = "4209"))
        buffMounts += MountItem(findWorkshopMaterial(item3.trmCode, materials), TrayMountData(item3.label, item3.trmCode).label, item3.kei, item3.count * item3.lenghtFactor * trayLenght, item3.isNeedLabel)

        val item4 = mountRules.find(s => s.label.equals("4210") && s.inputTypeIdRange.equals("63;")).getOrElse(TrayMountRules(label = "4210"))
        buffMounts += MountItem(findWorkshopMaterial(item4.trmCode, materials), TrayMountData(item4.label, item4.trmCode).label, item4.kei, item4.count * item4.lenghtFactor * trayLenght, item4.isNeedLabel)

        val item6 = mountData.find(s => s.typeId == 50 && s.matId == foranTray.materialId).getOrElse(TrayMountData(label = "NF"))
        buffMounts += MountItem(findWorkshopMaterial(item6.trmCode, materials), TrayMountData(item6.label, item6.trmCode).label, "796", 2 * trayLenght, true)

      } else {
        val item4 = mountRules.find(s => s.label.equals("4211")).getOrElse(TrayMountRules(label = "4211"))
        buffMounts += MountItem(findWorkshopMaterial(item4.trmCode, materials), TrayMountData(item4.label, item4.trmCode).label, item4.kei, item4.count * item4.lenghtFactor * trayLenght, item4.isNeedLabel)
      }
    }


    mountRules.filter(p => p.inputTypeIdRange.contains(clickTrayMontData.typeId.toString)).foreach(item => {
      if (item.trmCode.nonEmpty) {
        item.label match {

          case "4213" => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(item.label, item.trmCode).label, item.kei, item.count * item.lenghtFactor * 3 * clickTrayMontData.parD2 / 1000, item.isNeedLabel)

          case "4002" => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(item.label, item.trmCode).label, item.kei, item.count * item.lenghtFactor * 3 * clickTrayMontData.parD2 / 1000, item.isNeedLabel)


          case _ => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(item.label, item.trmCode).label, item.kei,  calculateQty(item.count , item.lenghtFactor , trayLenght), item.isNeedLabel)
        }

      } else {
        item.searchTypeIdRange match {
          case "51;" =>
            mountData.find(s => s.typeId == 51 && s.matId == clickTrayMontData.matId) match {
              case Some(value) => buffMounts += MountItem(findWorkshopMaterial(value.trmCode, materials), TrayMountData(value.label, value.trmCode).label, item.kei,  calculateQty(item.count , item.lenghtFactor , trayLenght), item.isNeedLabel)
              case None => MountItem()
            }

          case "65;" =>
            mountData.find(s => s.typeId == 65 && s.matId == clickTrayMontData.matId && s.parI1 == clickTrayMontData.parD2.toInt) match {
              case Some(value) => buffMounts += MountItem(findWorkshopMaterial(value.trmCode, materials), TrayMountData(value.label, value.trmCode).label, item.kei,  calculateQty(item.count , item.lenghtFactor , trayLenght), item.isNeedLabel)
              case None => MountItem()
            }
          case "66;" =>
            mountData.find(s => s.typeId == 66 && s.matId == clickTrayMontData.matId) match {
              case Some(value) => buffMounts += MountItem(findWorkshopMaterial(value.trmCode, materials), TrayMountData(value.label, value.trmCode).label, item.kei,  calculateQty(item.count , item.lenghtFactor , trayLenght), item.isNeedLabel)
              case None => MountItem()
            }
          case "57;" =>
            val it = mountData.filter(s => (s.typeId == 57) && s.matId == clickTrayMontData.matId && s.parD1 * 1000 >= foranTray.marign)
            if (it.nonEmpty) {
              val part = it.minBy(d => d.parI1)
              buffMounts += MountItem(findWorkshopMaterial(part.trmCode, materials), TrayMountData(part.label, part.trmCode).label, item.kei,  calculateQty(item.count , item.lenghtFactor , trayLenght), item.isNeedLabel)
            } else {
              MountItem()
            }
          case "72;" =>
            val it = mountData.filter(s => (s.typeId == 72) && s.parD2 >= clickTrayMontData.parD2)
            if (it.nonEmpty) {
              val part: TrayMountData = it.minBy(d => d.parD2)
              buffMounts += MountItem(findWorkshopMaterial(part.trmCode, materials), TrayMountData(part.label, part.trmCode).label, item.kei,  calculateQty(item.count , item.lenghtFactor , trayLenght), item.isNeedLabel)
            } else {
              MountItem()
            }
          case _ => None
        }
      }
    })


    Tray(foranTray, clickTrayMontData, material, buffMounts.toList)
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

