import org.scalatest.funsuite.AnyFunSuite
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import local.common.Codecs
import local.common.DBRequests.MountItem
import local.domain.WorkShopMaterial
import local.ele.CommonEle.{EleComplectParts, retrieveAllPartsFromJSON}
import local.pdf.ru.common.ReportCommon.{DocName, Item11Columns}
import local.pdf.ru.ele.EleEqTrayESKDReport
import local.pdf.ru.ele.EleEqTrayESKDReport.{generateElecPartsTojson, generatePdfToFileNoRev, generatePdfToFileWithRev}

import java.nio.file.Files
import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}


class TestElePdf extends AnyFunSuite with Codecs {

/*  val parts: EleComplectParts = retrieveAllPartsFromJSON("src/main/resources/test2.Json")
  val item11Columns: List[Item11Columns] = {
    val n1 = "Электрооборудование устанавливаемое заводом-строителем"
    val n2 = "Электрооборудование устанавливаемое электромонтажным предприятием"
    val buff = ListBuffer.empty[Item11Columns]
    val parttitions = parts.eqs.partition(x => x.workShopMaterial.singleWeight > 50)
    if (parttitions._1.nonEmpty) {
      buff += Item11Columns(true, n1.toUpperCase())
      parttitions._1.groupBy(s => s.SYSTEM_DESCR).toList.sortBy(s => s._1).foreach(gr => {
        buff += Item11Columns(true, gr._1)
        gr._2.sortBy(s => s.orderItem()).foreach(eq => {
          buff += Item11Columns(
            A1 = eq.LABEL,
            A2 = eq.USERID,
            A3 = eq.workShopMaterial.description,
            A4 = eq.workShopMaterial.name,
            A5 = eq.workShopMaterial.trmCode,
            A6 = eq.workShopMaterial.units,
            A7 = "1",
            A8 = String.format("%.2f",eq.workShopMaterial.singleWeight),
            A9 = String.format("%.2f",eq.workShopMaterial.singleWeight),
            A10 = eq.workShopMaterial.category,
            A11 = eq.ZONE_NAME

          )
          eq.SUPPORTS.sortBy(d => d.label).foreach(supp => {
            buff += Item11Columns(
              A1 = supp.label,
              A2 = "",
              A3 = supp.workShopMaterial.description,
              A4 = supp.workShopMaterial.name,
              A5 = supp.workShopMaterial.trmCode,
              A6 = supp.kei,
              A7 = supp.qty.toString,
              A8 = String.format("%.2f",supp.workShopMaterial.singleWeight),
              A9 = String.format("%.2f",(supp.workShopMaterial.singleWeight * supp.qty)),
              A10 = supp.workShopMaterial.category,
              A11 = eq.ZONE_NAME
            )
          })
        })
      })
    }
    if (parttitions._2.nonEmpty) {
      buff += Item11Columns(true, n2.toUpperCase())
      parttitions._2.groupBy(s => s.SYSTEM_DESCR).toList.sortBy(s => s._1).foreach(gr => {
        buff += Item11Columns(true, gr._1)
        gr._2.sortBy(s => s.orderItem()).foreach(eq => {
          buff += Item11Columns(
            A1 = eq.LABEL,
            A2 = eq.USERID,
            A3 = eq.workShopMaterial.description,
            A4 = eq.workShopMaterial.name, A5 = eq.workShopMaterial.trmCode,
            A6 = eq.workShopMaterial.units,
            A7 = "1",
            A8 = String.format("%.2f",eq.workShopMaterial.singleWeight),
            A9 = String.format("%.2f",eq.workShopMaterial.singleWeight),
            A10 = eq.workShopMaterial.category,
            A11 = eq.ZONE_NAME

          )
          eq.SUPPORTS.sortBy(d => d.label).foreach(supp => {
            buff += Item11Columns(
              A1 = supp.label,
              A2 = "",
              A3 = supp.workShopMaterial.description,
              A4 = supp.workShopMaterial.name,
              A5 = supp.workShopMaterial.trmCode,
              A6 = supp.kei,
              A7 = supp.qty.toString,
              A8 = String.format("%.2f",supp.workShopMaterial.singleWeight),
              A9 = String.format("%.2f",(supp.workShopMaterial.singleWeight * supp.qty)),
              A10 = supp.workShopMaterial.category,
              A11 = eq.ZONE_NAME
            )
          })
        })
      })
    }
    val supports: List[MountItem] = {
      val suppBuffer = ListBuffer.empty[MountItem]
      parts.trays.foreach(tr => {
        suppBuffer += MountItem(tr.workShopMaterial, tr.mountData.label, "006", tr.foranTray.LEN/1000)
        suppBuffer ++= tr.supports
      })
      suppBuffer.toList
    }

    val supportsRows: List[Item11Columns] = {
      val buffer = ListBuffer.empty[Item11Columns]
      supports.groupBy(s => s.workShopMaterial.trmCode).toList.foreach(group => {
        if (group._2.nonEmpty) {
          val item = group._2.head
          val label = item.label
          val kei = item.kei
          val qty: Double = Math.ceil(group._2.map(_.qty).sum)
          buffer += Item11Columns(
            A1 = label,
            A2 = "",
            A3 = item.workShopMaterial.description,
            A4 = item.workShopMaterial.name,
            A5 = item.workShopMaterial.trmCode,
            A6 = kei,
            A7 = String.format("%.2f",qty),
            A8 = String.format("%.2f",item.workShopMaterial.singleWeight),
            A9 = String.format("%.2f",(item.workShopMaterial.singleWeight * qty)),
            A10 = item.workShopMaterial.category,
            A11 = ""
          )
        }
      })
      buffer.toList
    }

    val gr4 = supportsRows.filter(p => p.A1.startsWith("4"))
    if (gr4.nonEmpty) {
      buff += Item11Columns(true, "Крепление и заземление кабелей")
      buff ++= gr4.sortBy(s => s.A1)
    }
    val gr5 = supportsRows.filter(p => p.A1.startsWith("5"))
    if (gr5.nonEmpty) {
      buff += Item11Columns(true, "Доизоляционные детали крепления")
      buff ++= gr5.sortBy(s => s.A1)
    }
    val gr6 = supportsRows.filter(p => p.A1.startsWith("6"))
    if (gr6.nonEmpty) {
      buff += Item11Columns(true, "Послеизоляционные детали крепления")
      buff ++= gr6.sortBy(s => s.A1)
    }
    val gr7 = supportsRows.filter(p => p.A1.startsWith("7"))
    if (gr7.nonEmpty) {
      buff += Item11Columns(true, "Трубы защиты кабеля")
      buff ++= gr7.sortBy(s => s.A1)
    }
    val gr8 = supportsRows.filter(p => !p.A1.startsWith("4") && !p.A1.startsWith("5") && !p.A1.startsWith("6") && !p.A1.startsWith("7"))
    if (gr8.nonEmpty) {
      buff += Item11Columns(true, "Прочее")
      buff ++= gr8.sortBy(s => s.A1)
    }
    buff.toList
  }
  val docName: DocName = DocName(num = parts.complect.drawingId, name = parts.complect.drawingDescr, lastRev = "2",userDev = "Сидоров")
  EleEqTrayESKDReport.genReport(docName, item11Columns,"C:/1")*/

  //val paths: List[String] =generatePdfToFileWithRev("P701","170701-884-5002","C:/5","2")//170701-884-2001 170701-884-5007

  val paths2: List[String] =generatePdfToFileWithRev("P701","170701-884-5008","C:/14","2")

  //val hh=generateElecPartsTojson("P701","170701-884-6008")

  val ret=0

}
