package orders

import deepsea.esp.{EspManager, EspManagerHelper}
import local.common.DBRequests.MaterialNode
import local.pdf.ru.order.OrderReportV1
import org.scalatest.funsuite.AnyFunSuite

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

class OrderPdfTest  extends AnyFunSuite with EspManagerHelper{

  val path: String =OrderReportV1.generateOrderPDF("200101","HUL","user")

  println(path)
  val h=0
}
