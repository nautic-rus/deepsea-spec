import local.hull.cnc.gcodePella.CNCManager.doCNCStrings
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, PrintWriter}
import scala.io.{BufferedSource, Source}

class CNCTestGcodesPella extends AnyFunSuite{
  val listIn: List[String] = List(
/*    "U102-01",
    "U102-02",
    "U102-03",
    "U102-04",
    "U102-05",
    "U102-06",

    "U102-08",
    "U102-09",
    "U102-10",
    "U102-11",*/
    "U0105_51_T",
    "U0105_51_0",
    "U102-07",
  )

  listIn.foreach(f=> {
    val src: BufferedSource = Source.fromFile("c:\\27\\102\\"+f+".txt")
    //val src: BufferedSource = Source.fromFile("c:\\32\\U306_11_0.txt")
    val lines: List[String] = src.getLines.toList
    src.close()
    //val ret: String = doCNC(lines, "c:\\26\\C-N004-ULTT-01"+f+".mpg", "Misha")
    val retStar = doCNCStrings(lines, "Misha")
    val pw = new PrintWriter(new File("c:\\32\\C-"+f+".TAP"))
    retStar.foreach(line => pw.println(line))
    pw.close()
  })
}
