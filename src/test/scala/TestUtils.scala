import local.ele.CommonEle.retrieveEleComplects
import local.ele.utils.EleUtils.fixFBS
import org.scalatest.funsuite.AnyFunSuite

class TestUtils  extends AnyFunSuite{
  //val s="jgfdjdfgjdsfdsf"
  //val t=s.contains(".")

/*  retrieveEleComplects("P701").foreach(c=>{
    fixFBS("P701",c.drawingId)
    println(c.drawingId)
  })*/

  fixFBS("P701","170701-884-2009")
  val j=0
}
