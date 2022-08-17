import deepsea.devices.DeviceManager.Accommodation
import deepsea.devices.{DeviceHelper, DeviceManager}
import deepsea.pipe.PipeManager.Material
import local.common.DBRequests.findChess
import local.domain.CommonTypes.DrawingChess
import local.pdf.en.accom.AccomReportEn.genAccomListEnPDF
import local.pdf.en.common.ReportCommonEN.Item11ColumnsEN
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer


class AccomReportTest extends AnyFunSuite with DeviceHelper {


  val cess: List[DrawingChess] =findChess("200101-304-0001","0")
  val docNumber = "200101-304-0001"
  val rev = "0"
  val docName: String=getSystemName(docNumber)
  val accoms: List[Accommodation] = getAccommodations(docNumber)
  val ret=genAccomListEnPDF(docNumber,docName,rev,accoms)
  println(ret)



}
