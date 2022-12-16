import local.common.Codecs
import local.hull.nest.CommonNest.NestMaterial
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

  //val t1=genMateriaAlllListJson("N004")

  //val ggs=genMateriaAlllList("N004").filter(s=>s.NEST_LENGTH==2950)

  //val d4=plateNestByMaterialsAndDims("N004",ggs)
  //val jdj=0

  //val ret=genAllPlateNest2Json("N004")

  val blocks: List[String] =genBlocks("N002")
  val ds=0;

  //insertLock("N004","MU10305","DEMO")

  //val jjs=plateNestByBlocksJson("N004",blocks.filter(s=>s.equals("U102")||s.equals("U104")))

  val jje=plateNestByMaterialsAndDims("N004",List(NestMaterial("A",10.0,3762,2500,"2026 MU20404",0)))

  val jjs=plateNestByBlocksJson("N004",blocks)

  val jj=0
}
