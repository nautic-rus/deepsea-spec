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

object TrayManager extends TrayHelper with Codecs{


  case class Tray(foranTray: ForanTray, mountData: TrayMountData, workShopMaterial: WorkShopMaterial, supports: List[MountItem])



  case class TrayMountData(label: String = "NF", trmCode: String = "NF", typeId: Int = 0, typeDescr: String = "NF", matId: Int = 0, matDescr: String = "NF",
                           H: Double = 0, parD1: Double = 0, parD2: Double = 0, parD3: Double = 0, parD4: Double = 0, parD5: Double = 0,
                           parI1: Int = 0, parI2: Int = 0, parS1: String = "", parS2: String = "", parB1: Boolean = true, parB2: Boolean = true)

  case class TrayMountRules(label: String, trmCode: String, inputTypeIdRange: String, inputTypeDescr: String, searchTypeIdRange: String, typeDescr: String,
                            kei: String, count: Double, lenghtFactor: Double, variant: Int = 1, isNeedLabel: Boolean = false)

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
                        marign: Int = 0,
                        materialId: Int = 0
                      )

  def trayLabels(project: String, trayIdSeq: String): List[String] = {
    val ret = ListBuffer.empty[String]
    val traysMountData=retrieveAllTrayMountData()
    val mountData: List[TrayMountData] = retrieveTraysMountDate()
    val mountRules: List[TrayMountRules] = retrieveTraysMountRules()
    val clickTray: ForanTray = TrayBySeqId(project, trayIdSeq)
    val materials = retrieveAllMaterialsByProject(project)
    val tray = calculateTrayMountDate(project, clickTray, mountData, mountRules, materials,traysMountData)
    ret += "V=" + clickTray.marign.toString
    ret += tray.mountData.label
    tray.supports.sortBy(d => d.label).foreach(s => ret += s.label)
    ret.toList
  }

  def tarysByZonesSystems(project: String, zones: List[String], systems: List[String]): List[Tray] = {
    val buff = ListBuffer.empty[Tray]
    val mountData: List[TrayMountData] = retrieveTraysMountDate()
    val mountRules: List[TrayMountRules] = retrieveTraysMountRules()
    val materials = retrieveAllMaterialsByProject(project)
    val traysMountData= retrieveAllTrayMountData()
    val f=0
    retrieveTraysByZoneNameAndSysName(project, zones, systems).foreach(clickTray => {
      buff += calculateTrayMountDate(project, clickTray, mountData, mountRules, materials,traysMountData)
    })
    buff.toList
  }

  def traysByComplect(project:String,complect:EleComplect): List[Tray] ={
    tarysByZonesSystems(project,complect.zoneNames,complect.systemNames)
  }

  def tarysByZonesSystemsJson(project: String, zones: List[String], systems: List[String]):String={
    tarysByZonesSystems(project, zones, systems).asJson.noSpaces
  }

  private def calculateTrayMountDate(project: String, foranTray: ForanTray, mountData: List[TrayMountData], mountRules: List[TrayMountRules], materials: List[WorkShopMaterial], traysMountData:List[TrayMountData]): Tray = {
    val clickTrayMontData: TrayMountData = retrieveTrayMountDataByTrm(foranTray.STOCK_CODE,traysMountData)
    val material = findWorkshopMaterial(foranTray.STOCK_CODE, materials)
    val buffMounts = ListBuffer.empty[MountItem]
    mountRules.filter(p => p.inputTypeIdRange.contains(clickTrayMontData.typeId.toString)).foreach(item => {
      if (item.trmCode.nonEmpty) {
        item.label match {

          case "4213" => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(item.label, item.trmCode).label, item.kei, item.count * item.lenghtFactor * 3 * clickTrayMontData.parD2 / 1000, item.isNeedLabel)

          case "4002" => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(item.label, item.trmCode).label, item.kei, item.count * item.lenghtFactor * 3 * clickTrayMontData.parD2 / 1000, item.isNeedLabel)

          case _ => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(item.label, item.trmCode).label, item.kei, item.count * item.lenghtFactor * foranTray.LEN / 1000, item.isNeedLabel)
        }

      } else {

        item.searchTypeIdRange match {
          case "51;" =>
            mountData.find(s => s.typeId == 51 && s.matId == clickTrayMontData.matId) match {
              case Some(value) => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(value.label, value.trmCode).label, item.kei, item.count * item.lenghtFactor * foranTray.LEN / 1000, item.isNeedLabel)
              case None => MountItem()
            }
          case "65;" =>
            mountData.find(s => s.typeId == 65 && s.matId == clickTrayMontData.matId && s.parI1 == clickTrayMontData.parD2.toInt) match {
              case Some(value) => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(value.label, value.trmCode).label, item.kei, item.count * item.lenghtFactor * foranTray.LEN / 1000, item.isNeedLabel)
              case None => MountItem()
            }
          case "43;" =>
            mountData.find(s => s.typeId == 43 && s.matId == clickTrayMontData.matId) match {
              case Some(value) => buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(value.label, value.trmCode).label, item.kei, item.count * item.lenghtFactor * foranTray.LEN / 1000, item.isNeedLabel)
              case None => MountItem()
            }
          case "57;" =>
            val it = mountData.filter(s => (s.typeId == 57) && s.matId == clickTrayMontData.matId && s.parD1 * 1000 >= foranTray.marign)
            if (it.nonEmpty) {
              val part = it.minBy(d => d.parI1)
              buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(part.label, part.trmCode).label, item.kei, item.count * item.lenghtFactor * foranTray.LEN / 1000, item.isNeedLabel)
            } else {
              MountItem()
            }
          case "72;" =>
            val it = mountData.filter(s => (s.typeId == 72) && s.parD2 >= clickTrayMontData.parD2)
            if (it.nonEmpty) {
              val part = it.minBy(d => d.parD2)
              buffMounts += MountItem(findWorkshopMaterial(item.trmCode, materials), TrayMountData(part.label, part.trmCode).label, item.kei, item.count * item.lenghtFactor * foranTray.LEN / 1000, item.isNeedLabel)
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

  def genTraysByZoneNameAndSysName(project: String = "P701", zones: List[String] = List.empty[String], systems: List[String] = List.empty[String]): Unit = retrieveTraysByZoneNameAndSysName(project, zones, systems)

  def genCablesInLineByTwoNodes(project: String, nodeName1: String, nodeName2: String): List[String] = cablesinLineByTwoNodeNames(project, nodeName1, nodeName2)

  def genTraysListByZoneNameAndSysName(project: String = "P701", zones: List[String] = List.empty[String], systems: List[String] = List.empty[String]): List[Tray] = {
    val buff = ListBuffer.empty[Tray]

    buff.toList
  }



}

