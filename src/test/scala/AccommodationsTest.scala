package scala

import cats.Show.Shown.mat
import deepsea.accomodations.AccommodationHelper
import deepsea.accomodations.AccommodationManager.Accommodation
import deepsea.database.DBManager
import deepsea.devices.DeviceManager
import deepsea.devices.DeviceManager.Device
import deepsea.devices.{DeviceHelper, DeviceManager}
import deepsea.materials.MaterialsHelper
import deepsea.pipe.PipeHelper
import deepsea.pipe.PipeManager.Material
import local.common.DBRequests.findChess
import local.domain.CommonTypes.DrawingChess
import local.pdf.en.accom.AccomReportEn.genAccomListEnPDF
import local.pdf.en.common.ReportCommonEN.Item11ColumnsEN
import org.mongodb.scala.MongoDatabase
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class AccommodationsTest extends AnyFunSuite with DeviceHelper with AccommodationHelper with MaterialsHelper{

  val mongo: Option[MongoDatabase] = DBManager.GetMongoConnection() //todo need for testing

  val materials: List[Material] = getMaterials

  val docNumber = "200101-525-004"
  val revision = "0"
  val docName: String = getASName(docNumber)
  val lang = "en"


  //val accommodations: List[Device] = (getDevices(docNumber) ++ getAccommodationsAsDevices(docNumber, lang)).sortBy(_.userId)
  val accommodations: List[Device] = getDevicesWithAccommodations(docNumber).sortBy(_.userId)

  accommodations.filter(_.desc2.contains("&")).foreach(d => {
    val ids = d.desc2.split("&")
    val accom = accommodations.filter(_.elemType == "accommodation").filter(x => ids.contains(x.userId))
    accom.foreach(x => x.userId = d.userId + "." + x.userId)
  })

  val jk1 = getAccommodationsAsDevices(docNumber, lang)

  val devices: List[Device] = getDevices(docNumber)

  val ret: String = genAccomListEnPDF(docNumber, docName, revision, accommodations, lang)


  println(ret)
  val  jk = 0

}
