import local.hull.cnc.hyprethermGcode.CNCManager.{doCNC, doCNCStrings}
import org.scalatest.funsuite.AnyFunSuite

import scala.io.{BufferedSource, Source}

class CNCTest extends AnyFunSuite{
 val src: BufferedSource = Source.fromFile("c:\\21\\testOffset.txt")
  val lines: List[String] = src.getLines.toList
  src.close()
 //val lines=List.empty[String]
  val ret: String =doCNC(lines,"c:\\21\\testOffset.mpg","Misha")

 //val hh=doCNCStrings(lines,"Kolya")

 val jj=0
}
