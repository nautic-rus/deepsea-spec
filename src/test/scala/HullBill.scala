import local.hull.bill.BillHelper
import local.hull.bill.BillManager.{genAnalyticPlateData, genAnalyticPlateDataJson, genAnalyticProfileData, genAnalyticProfileDataJson, genProfileNestBillJson, genWastsgeByParentKplJson}
import org.scalatest.funsuite.AnyFunSuite

class HullBill   extends AnyFunSuite with BillHelper{


/*  val pr=genProfileNestBill("N004")
  val pl=genPlateNestBill("N004")
  val j0=genTotPlates("N004")
  val j2= genTotProfiles("N004")*/

  //val jjs=genTotProfiles("N004").filter(d=>d.STOCK!=0.0)

 // val tty=genProfileNestBill("N004").filter(d=>d.STOCK!=0.0)

  //val fff=genAnalyticProfileData("N004").filter(d=>d.stock!=0.0)

  //val profBill=genProfileNestBillJson("N002")


  //val ap=genAnalyticProfileDataJson("N004")

  //val jjh=genWastsgeByParentKplJson("N004",1)

  val apl=genAnalyticPlateData("N002")//.filter(s=>s.KPL==100)


  val jj=0
}
