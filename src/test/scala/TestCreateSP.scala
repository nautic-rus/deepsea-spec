
import local.common.DBRequests.findChess
import local.domain.CommonTypes
import local.hull.BStree
import local.pdf.en.prd.PrdPartsReportEN.genHullPartListEnPDF
import org.apache.log4j.{Level, Logger}
import org.scalatest.funsuite.AnyFunSuite

class TestCreateSP extends AnyFunSuite with BStree {
  org.apache.log4j.BasicConfigurator.configure()

  //val bl = genBlocks("P701")

//  val pl: String =initHullPartList("N004", "NR004-150-101", "NR004-150-101", "Block 101", "ZAGU")

 //val pl: String =genPartList("NR004","U104","","","","")

 //val pl2: String =getHullPartList("NR004-150-101")

  //val testChess: List[CommonTypes.DrawingChess] =findChess("docNumber","2")

  genHullPartListEnPDF("N004","210101-102-0104", "170701-884-8001","0","c:\\14\\NR004-150-101.pdf")
  val jj = 0


}
