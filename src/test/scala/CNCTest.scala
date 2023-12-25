
import breeze.linalg
import breeze.linalg.{DenseVector, norm}


//import local.hull.cnc.pellaESSI.EssiCNCManager.doEssiCNC
import local.hull.cnc.pellaESSI.EssiCNCManagerSubdiv.doEssiCNC


import org.scalatest.funsuite.AnyFunSuite
import spire.std.ArraySupport.dot

import java.io.{File, PrintWriter}
import scala.io.{BufferedSource, Source}

class CNCTest extends AnyFunSuite {


  val listIn: List[String] = List(
    "DB_129_0"
  )



  listIn.foreach(f => {
    val src: BufferedSource = Source.fromFile("c:\\12\\" + f + ".txt")
    val lines: List[String] = src.getLines.toList
    src.close()
    val retStar: List[String] = doEssiCNC(lines,  "Misha")
    val pw = new PrintWriter(new File("c:\\12\\" + f + ".esi"))
    retStar.foreach(line => pw.println(line))
    pw.close()
  })



  val jj = 0
}
