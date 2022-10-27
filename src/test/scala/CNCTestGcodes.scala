import local.hull.cnc.hyprethermGcode.CNCManager.doCNCStrings
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, PrintWriter}
import scala.io.{BufferedSource, Source}

class CNCTestGcodes extends AnyFunSuite {
  val listIn: List[String] = List(
    "C-N004-U701-07_revA"
  )

  listIn.foreach(f=>{
  val src: BufferedSource = Source.fromFile("c:\\1\\"+f+".txt")
  //val src: BufferedSource = Source.fromFile("c:\\32\\U306_11_0.txt")
  val lines: List[String] = src.getLines.toList
  src.close()
  //val ret: String = doCNC(lines, "c:\\26\\C-N004-ULTT-01"+f+".mpg", "Misha")
  val retStar = doCNCStrings(lines, "Misha")
  val pw = new PrintWriter(new File("c:\\1\\"+f+".mpg"))
  retStar.foreach(line => pw.println(line))
  pw.close()

  })
}
