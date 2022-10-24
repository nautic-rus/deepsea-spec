import local.eqmount.EqFoundationManager
import local.eqmount.EqFoundationManager.{foranFoudationsAndEqs, foranFoudationsAndEqsJson, updateStatusSQL}
import org.scalatest.funsuite.AnyFunSuite

class EqFoundationTest extends AnyFunSuite{
  val rets=foranFoudationsAndEqs("N004")
  //val a=updateStatusSQL("N004",3401387,"Golenishev")

}
