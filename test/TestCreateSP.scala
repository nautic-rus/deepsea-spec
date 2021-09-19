
import local.hull.BStree
import org.apache.log4j.{Level, Logger}
import org.scalatest.funsuite.AnyFunSuite

class TestCreateSP extends AnyFunSuite with BStree {
  org.apache.log4j.BasicConfigurator.configure()
  Logger.getRootLogger.setLevel(Level.OFF)
  //val a = genPartList("N002", "U0105", "sad6", "43543", "name", "user")

  val bl = genBlocks("N002")
  val jj = 0


}
