package scala

import cats.Show.Shown.mat
import deepsea.accomodations.AccommodationHelper
import deepsea.accomodations.AccommodationManager.Accommodation
import deepsea.database.DBManager
import deepsea.devices.DeviceManager
import deepsea.devices.DeviceManager.Device
import deepsea.devices.{DeviceHelper, DeviceManager}
import deepsea.pipe.PipeHelper
import local.common.DBRequests.findChess
import local.domain.CommonTypes.DrawingChess
import local.pdf.en.accom.AccomReportEn.genAccomListEnPDF
import local.pdf.en.common.ReportCommonEN.Item11ColumnsEN
import org.mongodb.scala.MongoDatabase
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class AccommodationsTest extends AnyFunSuite with DeviceHelper with AccommodationHelper {

  val mongo: Option[MongoDatabase] = DBManager.GetMongoConnection() //todo need for testing

  val docNumber = "200101-522-002"
  val revision = "0"
  val docName: String = getASName(docNumber)
  val accommodations: List[Device] = getAccommodations(docNumber).filter(_.material.code != "").map(_.asDevice)
  val lang = "en"
  val ret: String = genAccomListEnPDF(docNumber, docName, revision, accommodations, "ru")

  val units = getUnits
  val jk1 = units

  //todo here you need to create a PDF method like the following one
  //todo genAccommodationsListPDF(docNumber, docName, revision, accommodations, lang)
  //todo which returns PDF file path generated with next rule
  //todo val filePath: String = Files.createTempDirectory("accommodationPdf").toAbsolutePath.toString + File.separator + docNumber + "_rev" + rev + ".pdf"





  val jk = 0

}
