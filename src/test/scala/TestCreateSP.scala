
import local.common.DBRequests.findChess
import local.domain.CommonTypes
import local.hull.{BStree, PartManager}
import local.hull.PartManager.ForanPartsByDrawingNum
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
  //val ret: List[PartManager.PrdPart] =ForanPartsByDrawingNum("N002", "200101-222-0105")
  genHullPartListEnPDF("SC01","300000-222-0105", "300000-222-0105","0","c:\\1\\1.pdf")
  val jj = 0


}
