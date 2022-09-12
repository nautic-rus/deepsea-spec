package scala

import deepsea.accomodations.AccommodationHelper
import deepsea.accomodations.AccommodationManager.Accommodation
import deepsea.database.DBManager
import deepsea.devices.DeviceManager
import deepsea.devices.DeviceManager.Device
import deepsea.devices.{DeviceHelper, DeviceManager}
import org.mongodb.scala.MongoDatabase
import org.scalatest.funsuite.AnyFunSuite

class AccommodationsTest extends AnyFunSuite with AccommodationHelper{

  val mongo: Option[MongoDatabase] = DBManager.GetMongoConnection() //todo need for testing

  val docNumber = "200101-522-002"
  val revision = "0"
  val docName: String = getASName(docNumber)
  val accommodations: List[Accommodation] = getAccommodations(docNumber).filter(_.material.code != "")
  val lang = "en"

  //todo here you need to create a PDF method like the following one
  //todo genAccommodationsListPDF(docNumber, docName, revision, accommodations, lang)
  //todo which returns PDF file path generated with next rule
  //todo val filePath: String = Files.createTempDirectory("accommodationPdf").toAbsolutePath.toString + File.separator + docNumber + "_rev" + rev + ".pdf"

  val jk = 0

}
