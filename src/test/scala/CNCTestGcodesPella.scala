import local.hull.cnc.gcodePella.CNCManager.doCNCStrings
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer
import scala.io.{BufferedSource, Source}

class CNCTestGcodesPella extends AnyFunSuite {
  val listIn: List[String] = List(
    /*"U0201_42_0_OLD",*/
    "U0204_50_0"

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


  listIn.foreach(f => {
    val src: BufferedSource = Source.fromFile("c:\\35\\" + f + ".txt")
    //val src: BufferedSource = Source.fromFile("c:\\32\\U306_11_0.txt")
    val lines: List[String] = src.getLines.toList
    src.close()
    //val ret: String = doCNC(lines, "c:\\26\\C-N004-ULTT-01"+f+".mpg", "Misha")
    val retStar = doCNCStrings(lines.toList, "Misha")
    val pw = new PrintWriter(new File("c:\\35\\" + f + ".TAP"))
    retStar.foreach(line => pw.println(line))
    pw.close()
  })

  def fixCUTL(in: List[String]): List[String] = {
    val out = ListBuffer.empty[String]
    var cutl = ""
    var jmp = ""
    in.foreach(str => {
      if (str.contains("JUMP")) {
        jmp = str.replace("JUMP ", "")
      }
      if (str.contains("CUTL")) {
        cutl = str.replace("CUTL ", "")
      }
      if (str.contains("CUT ") && cutl.nonEmpty) {
        val groups = str.split("  ")
        val nstr = groups(0) + "  " + groups(1) + "  " + jmp + " 999999 999999 " + groups(2)
        out+=nstr
        cutl = ""
        jmp=""
      }else{
        out+=str
      }
    })
    out.toList
  }
}
