package local.pdf.en.common

import com.itextpdf.io.font.constants.StandardFonts
import com.itextpdf.io.image.{ImageData, ImageDataFactory}
import com.itextpdf.kernel.font.{PdfFont, PdfFontFactory}
import com.itextpdf.kernel.pdf.PdfPage
import com.itextpdf.kernel.pdf.canvas.PdfCanvas
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Div, Image, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import local.pdf.UtilsPDF
import local.pdf.en.prd.PrdPartsReportEN.{dateNow, mmToPt}
import local.pdf.ru.common.ReportCommon.gostFont

import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala

object ReportCommonEN extends UtilsPDF {
  case class DocNameEN(code: String = "XXXXX", num: String = "210101-101-0103", name: String = "BLOCK 104. PART LIST.", lastRev: String = "0")
  case class Item11ColumnsEN(isHeader: Boolean = false, A1: String, A2: String = "", A3: String = "", A4: String = "", A5: String = "", A6: String = "", A7: String = "", A8: String = "", A9: String = "", A10: String = "", A11: String = "", project: String = "")

  val defaultFontSize: Float = mmToPt(3.5)

  def fontHELVETICA: PdfFont = PdfFontFactory.createFont(StandardFonts.HELVETICA)

  def fontCOURIER_BOLD: PdfFont = PdfFontFactory.createFont(StandardFonts.COURIER_BOLD)

  def getNnauticLigoEN: Image = {
    val imageData: ImageData = ImageDataFactory.create("src/main/resources/pict/nrlogo.png")
    new Image(imageData)
  }

  def border5mm(pdfPage: PdfPage): Unit = {
    val canvas = new PdfCanvas(pdfPage)
    canvas.rectangle(mmToPt(5), mmToPt(5), pdfPage.getDocument.getDefaultPageSize.getWidth - mmToPt(10), pdfPage.getDocument.getDefaultPageSize.getHeight - mmToPt(10))
    canvas.stroke()
  }

  def stampEN(): Table = {
    val cellBuff = ListBuffer.empty[Cell]
    val pointColumnWidths = Array(mmToPt(65), mmToPt(113), mmToPt(11), mmToPt(11))
    val table = new Table(pointColumnWidths)
    val tableWidthPt = mmToPt(200)
    val tableHeightPt = mmToPt(21)
    table.setWidth(tableWidthPt)
    table.setHeight(tableHeightPt)
    table.setHorizontalAlignment(HorizontalAlignment.CENTER)
    table.setVerticalAlignment(VerticalAlignment.MIDDLE)

    cellBuff += {
      new Cell(3, 0)
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


    cellBuff.foreach(cell => {
/*      cell.getChildren.asScala.head.asInstanceOf[Paragraph].getChildren.asScala.head.asInstanceOf[Text]
        .setText(cellBuff.indexOf(cell).toString)*/
      table.addCell(cell)
    })

    table.setFixedPosition(1, mmToPt(5), mmToPt(5), table.getWidth.getValue + 4)
    table
  }

  def fillStamp(doc: Document,docNameEN: DocNameEN, date: String = dateNow): Unit ={
    doc.add(genTextFixPos("SFI-DRAWING NO.", fontHELVETICA, 3.0f, 72f, 14f, 50f))
    doc.add(genTextFixPos("REV.", fontHELVETICA, 3.0f, 187.5f, 14f, 10f))
    doc.add(genTextFixPos("SHEET", fontHELVETICA, 3.0f, 196.5f, 14f, 10f))
    doc.add(genTextFixPos("DATE", fontHELVETICA, 2.5f, 185.5f, 21.0f, 10f))
    doc.add(genTextFixPos(date, fontHELVETICA, 4.0f, 188.0f, 17.0f, 20f))
    doc.add(genTextFixPos(docNameEN.lastRev, fontHELVETICA, 5.0f, 188.5f, 7f, 10f))
    doc.add(genTextFixPos(docNameEN.num, fontCOURIER_BOLD, 12.0f, 74.5f, 1.5f, 150f))
    doc.add(genTextFixPos(docNameEN.name, fontCOURIER_BOLD, 6f, 74.5f, 17.0f, 150f))
  }

  def genTextFixPos(content: String, font: PdfFont, fontSizeMM: Float, lMM: Float, bMM: Float, wMM: Float): Div = {
    val div = new Div().setMargin(0).setPadding(0).setKeepTogether(true)
    val txt = new Text(content).setFont(font).setFontSize(mmToPt(fontSizeMM))
    div.add(new Paragraph(txt))
    div.setFixedPosition(mmToPt(lMM), mmToPt(bMM), mmToPt(wMM))
    div
  }

}
