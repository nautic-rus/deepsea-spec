import deepsea.devices.DeviceManager.Accommodation
import deepsea.devices.{DeviceHelper}
import local.common.DBRequests.findChess
import local.domain.CommonTypes.DrawingChess
import local.pdf.en.accom.AccomReportEn.genAccomListEnPDF
import org.scalatest.funsuite.AnyFunSuite

class AccomReportTest extends AnyFunSuite with DeviceHelper {


  val cess: List[DrawingChess] =findChess("200101-304-0003","0")
  val docNumber = "200101-304-0003"
  val rev = "0"
  val docName: String=getSystemName(docNumber)
  val accoms: List[Accommodation] = getAccommodations(docNumber)
  val ret: String =genAccomListEnPDF(docNumber,docName,rev,accoms,"ru")
  println(ret)



}
