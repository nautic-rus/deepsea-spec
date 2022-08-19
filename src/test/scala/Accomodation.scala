package scala

import deepsea.database.DBManager
import deepsea.devices.DeviceManager.Accommodation
import deepsea.devices.{DeviceHelper, DeviceManager}
import org.mongodb.scala.MongoDatabase
import org.scalatest.funsuite.AnyFunSuite

class Accomodation extends AnyFunSuite with DeviceHelper{

  val mongo: Option[MongoDatabase] = DBManager.GetMongoConnection() //todo need for testing

  val docNumber = "200101-304-0001"
  val rev = "0"
  val docName: String = getSystemName(docNumber)
  val accomodations: List[Accommodation] = getAccommodations(docNumber)

  val jk = accomodations

  //todo createDevicesPDF(docNumber, docName, rev, devices)
  //todo метод для генерации PDF, входные параметры я тебе передам, параметры для теста выше
  //todo userId это номер позиции, в коллекцию devices уже включеные элементы из описаний формата "304.001|NR....|796|1", они определяются по параметру fromAux = 1

}
