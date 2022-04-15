import local.hull.cnc.hyprethermGcode.CNCManager.{doCNC, doCNCStrings}
import org.scalatest.funsuite.AnyFunSuite

import scala.io.{BufferedSource, Source}

class CNCTest extends AnyFunSuite{
 val src: BufferedSource = Source.fromFile("c:\\24\\TEST_ZERO.txt")
  val lines: List[String] = src.getLines.toList
  src.close()
 //val lines=List.empty[String]
  val ret: String =doCNC(lines,"c:\\24\\U703_02_0.mpg","Misha")

 //val hh=doCNCStrings(lines,"Kolya")

 val jj=0
}
