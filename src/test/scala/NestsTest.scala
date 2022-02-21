import local.common.Codecs
import local.hull.nest.{CommonNest, NestHelper}
import local.hull.nest.NestManager._
import org.scalatest.funsuite.AnyFunSuite

class NestsTest  extends AnyFunSuite with Codecs with NestHelper{
  //val nests=genAllPlateNestJson("N004")

  //val d=genBlocksJson("N004")

  //val d1=genAllPlateNestJson("N004")

  //val d2=genMateriaAlllListJson("N004")

  //val d3=genMaterialNyBlockJson("N004",genBlocks("N004"))

  //val d4=plateNestByMaterialsJson("N004",genMateriaAlllList("N004"))

  //val d4=plateNestByMaterialsAndDimsJson("N004",genMateriaAlllList("N004"))

  val blocks: List[String] =genBlocks("N004")

  val jjs=plateNestByBlocks("N004",blocks.filter(s=>s.equals("U102")))

  val jj=0
}
