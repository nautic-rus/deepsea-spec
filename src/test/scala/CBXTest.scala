import local.ele.cb.CableBoxHelper
import local.ele.cb.CableBoxManager.{cableBoxBySeqId, cableBoxBySeqIdJson}
import org.scalatest.funsuite.AnyFunSuite

class CBXTest extends AnyFunSuite with CableBoxHelper{
  //val cbx=cableBoxBySeqIdJson("P701","18621400")

  val ss=cableBoxBySeqId("P701","18579579")

  //val jj3=ForanCableBoxBySeqId("P701","18580379")
  val jj=0
}
