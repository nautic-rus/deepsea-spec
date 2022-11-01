import local.hull.cnc.gcodePella.CNCManager.doCNCStrings
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, PrintWriter}
import scala.io.{BufferedSource, Source}

class CNCTestGcodesPella extends AnyFunSuite{
  val listIn: List[String] = List(
    /*"U0201_42_0_OLD",*/
    "U0201_42_0_NEW"

/*    "C-N002-BS02-13",
    "C-N002-BS02-41",*/
   /* "U0103-04",
    "U0103-05",
    "U0103-06",
    "U0103-07",
    "U0103-08",
    "U0103-09",
    "U0103-10",

    "U0103-21",
    "U0103-22",
    "U0103-23",
    "U0103-24",
    "U0103-25",
    "U0103-26",
    "U0103-27",
    "U0103-28",
    "U0103-29",
    "U0103-30",

    "U0103-31",
    "U0103-32",
    "U0103-33",
    "U0103-34",
    "U0103-35",
    "U0103-36",
    "U0103-37",
*/


  )



  listIn.foreach(f=> {
    val src: BufferedSource = Source.fromFile("c:\\1\\"+f+".txt")
    //val src: BufferedSource = Source.fromFile("c:\\32\\U306_11_0.txt")
    val lines: List[String] = src.getLines.toList
    src.close()
    //val ret: String = doCNC(lines, "c:\\26\\C-N004-ULTT-01"+f+".mpg", "Misha")
    val retStar = doCNCStrings(lines, "Misha")
    val pw = new PrintWriter(new File("c:\\1\\"+f+".TAP"))
    retStar.foreach(line => pw.println(line))
    pw.close()
  })
}
