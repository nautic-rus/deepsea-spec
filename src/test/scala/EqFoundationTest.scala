import local.eqmount.EqFoundationManager
import local.eqmount.EqFoundationManager.{foranFoudationsAndEqs, foranFoudationsAndEqsJson, updateStatusSQL}
import org.scalatest.funsuite.AnyFunSuite

class EqFoundationTest extends AnyFunSuite{
  val rets=foranFoudationsAndEqs("N004")



  val jj=rets.filter(s=>s.foundationStatus==1)
  val h=0
  //val a=updateStatusSQL("N004",3401387,"Golenishev")

}
