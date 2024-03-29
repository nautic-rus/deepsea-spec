import local.qrutil.QrDxfHelper
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, FileWriter}

class TestQRDXF extends AnyFunSuite with QrDxfHelper {

  val str: String = url2qrDXF("https://en.wikipedia.org/wiki/QR_code")


  val fileWriter = new FileWriter(File.createTempFile("qr-code", ".dwg"))
  fileWriter.write(str);
  fileWriter.flush();
  fileWriter.close()

}
