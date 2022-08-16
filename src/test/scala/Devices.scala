package scala

import deepsea.database.DBManager
import deepsea.devices.DeviceManager.Device
import deepsea.devices.{DeviceHelper, DeviceManager}
import org.mongodb.scala.MongoDatabase
import org.scalatest.funsuite.AnyFunSuite

class Devices extends AnyFunSuite with DeviceHelper{

  val mongo: Option[MongoDatabase] = DBManager.GetMongoConnection() //todo need for testing



  val docNumber = "200101-304-0001"
  val rev = "0"
  val docName: String = getSystemName(docNumber)
  val devices: List[Device] = getDevices(docNumber)

  val jk = devices

  //todo createDevicesPDF(docNumber, docName, rev, devices)
  //todo метод для генерации PDF, входные параметры я тебе передам, параметры для теста выше

}
