import local.common.Codecs
import local.hull.nest.CommonNest
import local.hull.nest.NestManager.{genAllPlateNest, genAllPlateNestJson}
import org.scalatest.funsuite.AnyFunSuite

class NestsTest  extends AnyFunSuite with Codecs{
  val nests=genAllPlateNestJson("N004")

  val jj=0
}
