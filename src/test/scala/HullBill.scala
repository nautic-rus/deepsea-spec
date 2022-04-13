import local.hull.bill.BillHelper
import local.hull.bill.BillManager.{genAnalyticPlateDataJson, genAnalyticProfileData, genAnalyticProfileDataJson}
import org.scalatest.funsuite.AnyFunSuite

class HullBill   extends AnyFunSuite with BillHelper{


/*  val pr=genProfileNestBill("N004")
  val pl=genPlateNestBill("N004")
  val j0=genTotPlates("N004")
  val j2= genTotProfiles("N004")*/

  //val jjs=genTotProfiles("N004").filter(d=>d.STOCK!=0.0)

 // val tty=genProfileNestBill("N004").filter(d=>d.STOCK!=0.0)

  //val fff=genAnalyticProfileData("N004").filter(d=>d.stock!=0.0)

  //val ap=genAnalyticProfileDataJson("N004")


  val apl=genAnalyticPlateDataJson("N004")


  val jj=0
}
