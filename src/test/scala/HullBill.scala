import local.hull.bill.BillHelper
import org.scalatest.funsuite.AnyFunSuite

class HullBill   extends AnyFunSuite with BillHelper{

  val pr=genProfileNestBill("N004")

  val pl=genPlateNestBill("N004")

  val jj=0
}
