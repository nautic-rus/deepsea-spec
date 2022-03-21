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

  val ggs=genMateriaAlllList("N004").filter(s=>s.NEST_LENGTH==2950)

  val d4=plateNestByMaterialsAndDims("N004",ggs)
  val jdj=0

 // val blocks: List[String] =genBlocks("N004")

  //insertLock("N004","MU10305","DEMO")

  //val jjs=plateNestByBlocksJson("N004",blocks.filter(s=>s.equals("U103")))

  val jj=0
}
