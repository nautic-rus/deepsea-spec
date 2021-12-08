package local.ele.trays

import local.domain.WorkShopMaterial

import scala.collection.mutable.ListBuffer

object TrayManager extends TrayHelper {


  case class Tray(foranTray: ForanTray, mountData: TrayMountData, workShopMaterial: WorkShopMaterial, supports: List[TrayMountItem])

  case class TrayMountItem(workShopMaterial: WorkShopMaterial = new WorkShopMaterial(), mountData: TrayMountData = TrayMountData(), kei: String = "", qty: Double = 0, isNeedLabel: Boolean = false)

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
    val mountData: List[TrayMountData] = retrieveTraysMountDate()
    val mountRules: List[TrayMountRules] = retrieveTraysMountRules()
    val clickTray: ForanTray = TrayBySeqId(project, trayIdSeq) //"17683679"
    val clickTrayMontData = retrieveTrayMountDataByTrm(clickTray.STOCK_CODE)
    ret += "V="+clickTrayMontData.H.toInt.toString
    ret += clickTrayMontData.label

    val buffMounts = ListBuffer.empty[TrayMountItem]
    mountRules.filter(p => p.inputTypeIdRange.contains(clickTrayMontData.typeId.toString)).foreach(item => {
      if (item.trmCode.nonEmpty) {
        item.label match {

          case "4213" => buffMounts += TrayMountItem(retrieveMaterialByTrm(project, item.trmCode), TrayMountData(item.label, item.trmCode), item.kei, item.count * item.lenghtFactor * 3 * clickTrayMontData.parD2 / 1000, item.isNeedLabel)

          case "4002" => buffMounts += TrayMountItem(retrieveMaterialByTrm(project, item.trmCode), TrayMountData(item.label, item.trmCode), item.kei, item.count * item.lenghtFactor * 3 * clickTrayMontData.parD2 / 1000, item.isNeedLabel)

          case _ => buffMounts += TrayMountItem(retrieveMaterialByTrm(project, item.trmCode), TrayMountData(item.label, item.trmCode), item.kei, item.count * item.lenghtFactor * clickTray.LEN / 1000, item.isNeedLabel)
        }

      } else {

        item.searchTypeIdRange match {
          case "54;" =>
            mountData.find(s => s.typeId == 54 && s.matId == clickTrayMontData.matId) match {
              case Some(value) => buffMounts += TrayMountItem(retrieveMaterialByTrm(project, value.trmCode), TrayMountData(value.label, value.trmCode), item.kei, item.count * item.lenghtFactor * clickTray.LEN / 1000, item.isNeedLabel)
              case None => TrayMountItem()
            }
          case "65;" =>
            mountData.find(s => s.typeId == 65 && s.matId == clickTrayMontData.matId && s.parI1 == clickTrayMontData.parD2.toInt) match {
              case Some(value) => buffMounts += TrayMountItem(retrieveMaterialByTrm(project, value.trmCode), TrayMountData(value.label, value.trmCode), item.kei, item.count * item.lenghtFactor * clickTray.LEN / 1000, item.isNeedLabel)
              case None => TrayMountItem()
            }
          case "43;" =>
            mountData.find(s => s.typeId == 43 && s.matId == clickTrayMontData.matId) match {
              case Some(value) => buffMounts += TrayMountItem(retrieveMaterialByTrm(project, value.trmCode), TrayMountData(value.label, value.trmCode), item.kei, item.count * item.lenghtFactor * clickTray.LEN / 1000, item.isNeedLabel)
              case None => TrayMountItem()
            }
          case "57;58;" =>
            val it = mountData.filter(s => (s.typeId == 57 || s.typeId == 58) && s.matId == clickTrayMontData.matId && s.parD1 * 1000 >= clickTray.marign)
            if (it.nonEmpty) {
              val part = it.minBy(d => d.parI1)
              buffMounts += TrayMountItem(retrieveMaterialByTrm(project, part.trmCode), TrayMountData(part.label, part.trmCode), item.kei, item.count * item.lenghtFactor * clickTray.LEN / 1000, item.isNeedLabel)
            } else {
              TrayMountItem()
            }
          case "72" =>
            val it = mountData.filter(s => (s.typeId == 72) && s.parD2 >= clickTrayMontData.parD2)
            if (it.nonEmpty) {
              val part = it.minBy(d => d.parD2)
              buffMounts += TrayMountItem(retrieveMaterialByTrm(project, part.trmCode), TrayMountData(part.label, part.trmCode), item.kei, item.count * item.lenghtFactor * clickTray.LEN / 1000, item.isNeedLabel)
            } else {
              TrayMountItem()
            }
          case _ => None
        }
      }
    })
    buffMounts.filter(b => b.isNeedLabel).sortBy(d => d.mountData.label).foreach(s => {
      ret += s.mountData.label
    })
    ret.toList
  }


  def genCablesByTraySeqId(project: String, trayIdSeq: String): List[String] = cablesByTraySeqId(project, trayIdSeq)


}

