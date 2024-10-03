
import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf.{PdfDocument, PdfReader, PdfWriter, WriterProperties}
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Paragraph, Table}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import local.common.DBRequests.findChess
import local.domain.CommonTypes
import local.hull.{BStree, PartManager}
import local.hull.PartManager.ForanPartsByDrawingNum
import local.pdf.UtilsPDF
import local.pdf.en.common.ReportCommonEN.{DocNameEN, border5mm, defaultFontSize, fillStamp, fontCOURIER_BOLD, fontHELVETICA, genTextFixPos, getNnauticLigoEN, stampEN}
import local.pdf.en.prd.PrdPartsReportEN.genHullPartListEnPDF
import local.pdf.ru.common.ReportCommon.gostFont
import org.apache.log4j.{Level, Logger}
import org.scalatest.funsuite.AnyFunSuite

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import scala.collection.mutable.ListBuffer

class TestCreateSP extends AnyFunSuite with BStree with UtilsPDF{
  org.apache.log4j.BasicConfigurator.configure()

  //val bl = genBlocks("P701")

//  val pl: String =initHullPartList("N004", "NR004-150-101", "NR004-150-101", "Block 101", "ZAGU")

 //val pl: String =genPartList("NR004","U104","","","","")

 //val pl2: String =getHullPartList("NR004-150-101")

  //val testChess: List[CommonTypes.DrawingChess] =findChess("docNumber","2")
  //val ret: List[PartManager.PrdPart] =ForanPartsByDrawingNum("N002", "200101-222-BS12")

  //genHullPartListEnPDF("SC01","300000-222-0105", "300000-222-0105","0","c:\\1\\1.pdf")
  val t=0
/*
  private val pageSize: PageSize = PageSize.A4

  val pdfWriter: PdfWriter = new PdfWriter("c:\\1\\1.pdf", new WriterProperties().setFullCompressionMode(true)) {
    setSmartMode(true)
  }

  val pdfDoc = new PdfDocument(pdfWriter) {
    initializeOutlines()
  }
  val doc: Document = new Document(pdfDoc, pageSize)
  val docNameEN: DocNameEN=new DocNameEN(name = "Система воздушных и переливных труб")
  val titul: PdfDocument = genTitulA4(docNameEN)
  titul.copyPagesTo(1, 1, pdfDoc)
  doc.close()
  private def genTitulA4(docNameEN: DocNameEN, date: String = dateNow): PdfDocument = {
    val os: OutputStream = new ByteArrayOutputStream()
    val pdfWriter = new PdfWriter(os)
    val pdfDoc = new PdfDocument(pdfWriter)
    val doc: Document = new Document(pdfDoc, pageSize) {
      setMargins(0, 0, 0, 0)
    }
    val width: Float = doc.getPdfDocument.getDefaultPageSize.getWidth
    val height: Float = doc.getPdfDocument.getDefaultPageSize.getHeight
    val logo = getNnauticLigoEN.scaleToFit(72, 20).setFixedPosition(mmToPt(9), mmToPt(8))
    doc.add(logo)
    border5mm(pdfDoc.getPage(1))
    val stamp: Table = stampENNew()
    fillStampNew(doc, docNameEN)
    doc.add(stamp)
    doc.close()
    os.flush()
    os.close()
    new PdfDocument(new PdfReader(new ByteArrayInputStream(os.asInstanceOf[ByteArrayOutputStream].toByteArray)))
  }
  def stampENNew(): Table = {
    val cellBuff = ListBuffer.empty[Cell]
    val pointColumnWidths = Array(mmToPt(27), mmToPt(151), mmToPt(11), mmToPt(11))
    val table = new Table(pointColumnWidths)
    val tableWidthPt = mmToPt(200)
    val tableHeightPt = mmToPt(21)
    table.setWidth(tableWidthPt)
    table.setHeight(tableHeightPt)
    table.setHorizontalAlignment(HorizontalAlignment.CENTER)
    table.setVerticalAlignment(VerticalAlignment.MIDDLE)

    cellBuff += {
      new Cell(0, 2)
        .setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph("")
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(gostFont)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(7))
    }
    cellBuff += {
      new Cell(0, 2)
        .setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph("")
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(gostFont)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(7))
    }
    cellBuff += {
      new Cell(2, 0)
        .setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph("")
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(gostFont)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(14))
    }
    cellBuff += {
      new Cell(2, 0)
        .setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph("")
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(gostFont)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(14))
    }
    cellBuff += {
      new Cell(0, 0)
        .setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph("")
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(gostFont)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }
    cellBuff += {
      new Cell(0, 0)
        .setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph("")
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(gostFont)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }
    cellBuff += {
      new Cell(0, 0)
        .setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph("")
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(gostFont)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(9))
    }
    cellBuff += {
      new Cell(0, 0)
        .setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph("")
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(gostFont)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(9))
    }

    cellBuff.foreach(cell => {
      table.addCell(cell)
    })

    table.setFixedPosition(1, mmToPt(5), mmToPt(5), table.getWidth.getValue + 4)
    table
  }
  def fillStampNew(doc: Document, docNameEN: DocNameEN, date: String = dateNow): Unit = {
    doc.add(genTextFixPos("SFI-DRAWING NO.", fontHELVETICA, 3.0f, 35f, 14f, 50f))
    doc.add(genTextFixPos("REV.", fontHELVETICA, 3.0f, 187.5f, 14f, 10f))
    doc.add(genTextFixPos("SHEET", fontHELVETICA, 3.0f, 196.5f, 14f, 10f))
    doc.add(genTextFixPos("DATE", fontHELVETICA, 2.5f, 185.5f, 21.0f, 10f))
    doc.add(genTextFixPos(date, fontHELVETICA, 4.0f, 188.0f, 17.0f, 20f))
    doc.add(genTextFixPos(docNameEN.lastRev, fontHELVETICA, 5.0f, 188.5f, 7f, 10f))
    doc.add(genTextFixPos(docNameEN.num, fontCOURIER_BOLD, 12.0f, 32.5f, 0.5f, 150f))
    doc.add(genTextFixPos(docNameEN.name, fontCOURIER_BOLD, 4f, 6.0f, 18.0f, 150f))
  }

*/

  val jj = 0


}
