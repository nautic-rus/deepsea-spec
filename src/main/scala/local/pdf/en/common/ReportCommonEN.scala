package local.pdf.en.common

import com.itextpdf.io.font.PdfEncodings
import com.itextpdf.io.font.constants.StandardFonts
import com.itextpdf.io.image.{ImageData, ImageDataFactory}
import com.itextpdf.kernel.font.{PdfFont, PdfFontFactory}
import com.itextpdf.kernel.pdf.PdfPage
import com.itextpdf.kernel.pdf.canvas.PdfCanvas
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Div, Image, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import local.pdf.UtilsPDF
import local.pdf.ru.common.ReportCommon.gostFont

import java.io.InputStream
import scala.collection.mutable.ListBuffer


object ReportCommonEN extends UtilsPDF {
  case class DocNameEN(code: String = "XXXXX", num: String = "210101-101-0103", name: String = "BLOCK 104. PART LIST.", lastRev: String = "0", user: String = "")

  case class Item11ColumnsEN(isHeader: Boolean = false, A1: String, A2: String = "", A3: String = "", A4: String = "", A5: String = "", A6: String = "", A7: String = "", A8: String = "", A9: String = "", A10: String = "", A11: String = "", project: String = "", A12: String = "")

  val defaultFontSize: Float = mmToPt(3.5)

  val helveticaBytes: Array[Byte] = {
    val fontURL = this.getClass.getResource("/fonts/ArialCyr.ttf");
    val fontStream: InputStream = fontURL.openStream()
    val b: Array[Byte] = fontStream.readAllBytes()
    fontStream.close()
    b
  }

  val courierCBytes: Array[Byte] = {
    val fontURL = this.getClass.getResource("/fonts/courierC.ttf");
    val fontStream: InputStream = fontURL.openStream()
    val b: Array[Byte] = fontStream.readAllBytes()
    fontStream.close()
    b
  }

  def fontHELVETICA: PdfFont = PdfFontFactory.createFont(helveticaBytes, "Cp1251") // PdfEncodings.WINANSI)

  def fontCOURIER_BOLD: PdfFont = PdfFontFactory.createFont(courierCBytes, "Cp1251") // PdfEncodings.WINANSI)

  def getNnauticLigoEN: Image = {
    val imageData: ImageData = ImageDataFactory.create("src/main/resources/pict/nrlogo.png")
    new Image(imageData)
  }

  def border5mm(pdfPage: PdfPage): Unit = {
    val canvas = new PdfCanvas(pdfPage)
    canvas.rectangle(mmToPt(5), mmToPt(5), pdfPage.getDocument.getDefaultPageSize.getWidth - mmToPt(10), pdfPage.getDocument.getDefaultPageSize.getHeight - mmToPt(10))
    canvas.stroke()
  }

  def stampENOld(): Table = {
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

  def fillStampOld(doc: Document, docNameEN: DocNameEN, date: String = dateNow): Unit = {
    doc.add(genTextFixPos("SFI-DRAWING NO.", fontHELVETICA, 3.0f, 72f, 14f, 50f))
    doc.add(genTextFixPos("REV.", fontHELVETICA, 3.0f, 187.5f, 14f, 10f))
    doc.add(genTextFixPos("SHEET", fontHELVETICA, 3.0f, 196.5f, 14f, 10f))
    doc.add(genTextFixPos("DATE", fontHELVETICA, 2.5f, 185.5f, 21.0f, 10f))
    doc.add(genTextFixPos(date, fontHELVETICA, 4.0f, 188.0f, 17.0f, 20f))
    doc.add(genTextFixPos(docNameEN.lastRev, fontHELVETICA, 5.0f, 188.5f, 7f, 10f))
    doc.add(genTextFixPos(docNameEN.num, fontCOURIER_BOLD, 12.0f, 74.5f, 1.5f, 150f))
    doc.add(genTextFixPos(docNameEN.name, fontCOURIER_BOLD, 6f, 74.5f, 17.0f, 150f))
  }

  def stamp(): Table = {
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

    table
  }
  def stampW(): Table = {
    val cellBuff = ListBuffer.empty[Cell]
    val pointColumnWidths = Array(mmToPt(27), mmToPt(151), mmToPt(13), mmToPt(13))
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

    table
  }

  def stampEN(): Table = {
    val table = stamp()
    table.setFixedPosition(1, mmToPt(5), mmToPt(5), table.getWidth.getValue + 4)
    table
  }
  def stampENH(): Table = {
    val table = stampW()
    table.setFixedPosition(1, mmToPt(92), mmToPt(5), table.getWidth.getValue + 4)
    table
  }

  def stampENLandscape(): Table = {
    val table = stamp()
    table.setFixedPosition(1, mmToPt(215), mmToPt(5), table.getWidth.getValue + 4)
    table
  }

  def fillStampLandscape(doc: Document, docNameEN: DocNameEN, date: String = dateNow, lang: String = "ru"): Unit = {
    val offset = 210.0f
    doc.add(genTextFixPos(if (lang == "ru") "НОМЕР ЧЕРТЕЖА" else "SFI-DRAWING NO.", fontHELVETICA, 3.0f, offset + 32.5f, 13.5f, 50f))
    doc.add(genTextFixPos(if (lang == "ru") "РЕВ." else "REV.", fontHELVETICA, 3.0f, offset + 187.5f, 13.3f, 10f))
    doc.add(genTextFixPos(if (lang == "ru") "ЛИСТ" else "SHEET", fontHELVETICA, 3.0f, offset + 197f, 13.3f, 10f))
    doc.add(genTextFixPos(if (lang == "ru") "ДАТА" else "DATE", fontHELVETICA, 2.5f, offset + 185.5f, 21.0f, 10f))
    doc.add(genTextFixPos(date, fontHELVETICA, 4.0f, offset + 188.0f, 17.0f, 20f))
    doc.add(genTextFixPos(docNameEN.lastRev, fontHELVETICA, 5.0f, offset + 188.5f, 5f, 10f))
    doc.add(genTextFixPos(docNameEN.num, fontCOURIER_BOLD, 12.0f, offset + 32.5f, 0.5f, 150f))
    doc.add(genTextFixPos(docNameEN.name, fontCOURIER_BOLD, 4f, offset + 6.0f, 18.0f, 150f))
  }

  def fillStamp(doc: Document, docNameEN: DocNameEN, date: String = dateNow, lang: String = "ru", department: String = "hull"): Unit = {
    var numToDisplay = docNameEN.num // Копируем значение, чтобы не изменять исходный docNameEN

    if (department == "hull" && docNameEN.num.contains("240402-210")) {
      val parts = docNameEN.num.split("-")
      if (parts.length >= 2) {
        val lastPart = parts.last
        numToDisplay = s"ОР.240402.21.${lastPart}"
      }
    }
    doc.add(genTextFixPos(if (lang == "ru") "НОМЕР ЧЕРТЕЖА" else "SFI-DRAWING NO.", fontHELVETICA, 3.0f, 35f, 14f, 50f))
    doc.add(genTextFixPos(if (lang == "ru") "РЕВ." else "REV.", fontHELVETICA, 3.0f, 187.5f, 13f, 10f))
    doc.add(genTextFixPos(if (lang == "ru") "ЛИСТ" else "SHEET", fontHELVETICA, 3.0f, 197f, 13f, 10f))
    doc.add(genTextFixPos(if (lang == "ru") "ДАТА" else "DATE", fontHELVETICA, 2.5f, 185.5f, 21.0f, 10f))
    doc.add(genTextFixPos(date, fontHELVETICA, 4.0f, 188.0f, 17.0f, 20f))
    doc.add(genTextFixPos(docNameEN.lastRev, fontHELVETICA, 5.0f, 188.5f, 7f, 10f))
    doc.add(genTextFixPos(numToDisplay, fontCOURIER_BOLD, 12.0f, 32.5f, 0.5f, 180f))
    doc.add(genTextFixPos(docNameEN.name, fontCOURIER_BOLD, 4f, 6.5f, 18.0f, 180f))
  }

  def fillStampH(doc: Document, docNameEN: DocNameEN, date: String = dateNow, lang: String = "ru"): Unit = {
    doc.add(genTextFixPos("DRAWING NUMBER / НОМЕР ЧЕРТЕЖА", fontHELVETICA, 3.0f, 120f, 14f, 50f))
    doc.add(genTextFixPos("REV. / РЕВ.", fontHELVETICA, 2.5f, 269.3f, 13.5f, 20f))
    doc.add(genTextFixPos("PAGE / ЛИСТ", fontHELVETICA, 2.5f, 280.8f, 13.5f, 25f))
    doc.add(genTextFixPos("DATE / ДАТА", fontHELVETICA, 2.5f, 269f, 21.0f, 25f))

    doc.add(genTextFixPos(date, fontHELVETICA, 4.0f, 274f, 17.0f, 20f))
    doc.add(genTextFixPos(docNameEN.lastRev, fontHELVETICA, 5.0f, 273f, 5f, 10f))
    doc.add(genTextFixPos(docNameEN.num, fontCOURIER_BOLD, 12.0f, 120.5f, 0.5f, 180f))
    doc.add(genTextFixPos(docNameEN.name, fontCOURIER_BOLD, 4f, 94f, 18.0f, 180f))
  }


  def genTextFixPos(content: String, font: PdfFont, fontSizeMM: Float, lMM: Float, bMM: Float, wMM: Float): Div = {
    val div = new Div().setMargin(0).setPadding(0).setKeepTogether(true)
    val txt = new Text(content).setFont(font).setFontSize(mmToPt(fontSizeMM))
    div.add(new Paragraph(txt))
    div.setFixedPosition(mmToPt(lMM), mmToPt(bMM), mmToPt(wMM))
    div
  }

  def addZeros(in: String): String = {
    if (in.contains(".")) {
      val splited = in.split("\\.")
      splited.head.length match {
        case 1 => "00" + in
        case 2 => "0" + in
        //case 3 => "0" + in
        case _ => in
      }

    } else {
      in.length match {
        case 1 => "00" + in
        case 2 => "0" + in
        //case 3 => "0" + in
        case _ => in
      }
    }
  }

  def rowwrap(in: String, maxcount: Int): List[String] = {
    val buff = ListBuffer.empty[String]
    var str = ""
    in.split(" ").foreach(w => {
      if (str.length + w.length < maxcount) {
        str = str + w + " "
      } else {

        buff += str.trim
        str = w + " "
      }
    })
    if (str.nonEmpty) buff += str.trim
    buff.toList
  }


}
