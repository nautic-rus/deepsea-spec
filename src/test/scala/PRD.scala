import local.hull.PartManager._
import org.scalatest.funsuite.AnyFunSuite

class PRD  extends AnyFunSuite {

  //val s=ForanPartLabelByDrawingNumAndPartName("N004","NR004-101-0103","0001")
  val sы=genForanPartLabelByDrawingNumAndPartNameJSON("N004","NR004-101-0103","0001")
  val b=genForanPartsByDrawingNumJSON("N004","NR004-101-0103")
  //val b=genForanPartsByDrawingNum("N004","NR004-101-0103")

  val cs=0

}
