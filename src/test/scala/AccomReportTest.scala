
import deepsea.devices.DeviceHelper
import deepsea.devices.DeviceManager.Device
import local.common.DBRequests.findChess
import local.domain.CommonTypes.DrawingChess
import local.pdf.en.accom.AccomReportEn.genAccomListEnPDF
import org.scalatest.funsuite.AnyFunSuite

class AccomReportTest extends AnyFunSuite with DeviceHelper {
  // 200101-435-001

/*  val cess: List[DrawingChess] =findChess("200101-304-0003","0")
  val docNumber = "200101-304-001"//00101-304-0001
  val rev = "0"
  val docName: String=getSystemName(docNumber)
  val accoms: List[Device] = getDevices(docNumber)
  val ret: String =genAccomListEnPDF(docNumber,docName,rev,accoms,"ru")
  println(ret)*/


  val cess: List[DrawingChess] = findChess("200101-435-001", "0")
  val docNumber = "200101-435-001" //00101-304-0001
  val rev = "0"
  val docName: String = getSystemName(docNumber)
  val accoms: List[Device] = getDevices(docNumber)
  val ret: String = genAccomListEnPDF(docNumber, docName, rev, accoms, "ru")
  println(ret)



}
