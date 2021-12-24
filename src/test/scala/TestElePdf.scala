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

import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}


class TestElePdf extends AnyFunSuite with Codecs {

  val parts: EleComplectParts = retrieveAllPartsFromJSON("src/main/resources/test2.Json")

  val item11Columns: List[Item11Columns] = {

    val suppBuffer=ListBuffer.empty[MountItem]
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
            A4 = eq.workShopMaterial.name, A5 = eq.workShopMaterial.trmCode,
            A6 = eq.workShopMaterial.units,
            A7 = "1",
            A8 = eq.workShopMaterial.singleWeight.toString,
            A9 = eq.workShopMaterial.singleWeight.toString,
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
              A8 = eq.workShopMaterial.singleWeight.toString,
              A9 = (eq.workShopMaterial.singleWeight * supp.qty).toString,
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
            A8 = eq.workShopMaterial.singleWeight.toString,
            A9 = eq.workShopMaterial.singleWeight.toString,
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
              A8 = eq.workShopMaterial.singleWeight.toString,
              A9 = (eq.workShopMaterial.singleWeight * supp.qty).toString,
              A10 = supp.workShopMaterial.category,
              A11 = eq.ZONE_NAME
            )
          })
        })
      })
    }

    parts.trays.foreach(tr => {
      suppBuffer+=MountItem(tr.workShopMaterial,tr.mountData.label,"006", tr.foranTray.LEN)
      suppBuffer++=tr.supports
    })



    buff.toList
  }


  val docName: DocName = DocName(num = parts.complect.drawingId, name = parts.complect.drawingDescr, lastRev = "2")
  EleEqTrayESKDReport.genReport(docName, item11Columns)

  val hh = 0

}
