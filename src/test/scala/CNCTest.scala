import local.hull.cnc.hyprethermGcode.CNCManager.doCNC
import org.scalatest.funsuite.AnyFunSuite

import scala.io.{BufferedSource, Source}

class CNCTest extends AnyFunSuite{
  val src: BufferedSource = Source.fromFile("c:\\21\\TEST_NEST_6_1\\U702_TEST_0.txt")
  val lines: List[String] = src.getLines.toList
  src.close()
  doCNC(lines,"c:\\21\\test.mpg","Kolya")
}
