package local.pdf.en.pipe

import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf.{PdfDocument, PdfReader, PdfWriter, WriterProperties}
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Div, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import deepsea.pipe.PipeHelper
import deepsea.pipe.PipeManager.{Material, PipeSeg}
import local.pdf.UtilsPDF
import local.pdf.en.common.ReportCommonEN.{DocNameEN, Item11ColumnsEN, border5mm, defaultFontSize, fillStamp, fontHELVETICA, getNnauticLigoEN, stampEN}

import scala.jdk.CollectionConverters.CollectionHasAsScala
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, OutputStream}
import java.nio.file.Files
import scala.collection.mutable.ListBuffer

object SpoolsReportEN extends UtilsPDF with PipeHelper {

  private val pageSize: PageSize = PageSize.A4

  private val pointColumnWidths = Array(
    mmToPt(15),
    mmToPt(134),
    mmToPt(15),
    mmToPt(20),
    mmToPt(20) + 0 //207
  )


  def genSpoolsListEnPDF(docNumber: String, docName: String, rev: String, rawData: List[PipeSeg]): String = {
    val filePath: String = Files.createTempDirectory("spoolPdf").toAbsolutePath.toString + File.separator + docNumber + "ML_rev" + rev + ".pdf"
    val rows: List[Item11ColumnsEN] = genRows(rawData)
    val totalRows: List[Item11ColumnsEN] = genTotal(rows)
    val dn = DocNameEN(num=docNumber, name=docName, lastRev = if (rev != "") rev else "0")
    processPDF(dn, filePath, rows, totalRows)

    filePath
  }

  def genSpoolsListEnPDFAll(docNumber: String, docName: String, rev: String, rawData: List[PipeSeg]): String = {
    val filePath: String = Files.createTempDirectory("spoolPdf").toAbsolutePath.toString + File.separator + docNumber + "ML_rev" + rev + ".pdf"
    val rows: List[Item11ColumnsEN] = genRows(rawData)
    val totalRows: List[Item11ColumnsEN] = genTotal(rows)
    val dn = DocNameEN(num=docNumber, name=docName, lastRev = if (rev != "") rev else "0")
    processPDF(dn, filePath, rows, totalRows, genAll = true)
    filePath
  }

  private def processPDF(docNameEN: DocNameEN, path: String, items: List[Item11ColumnsEN], totalItems: List[Item11ColumnsEN], genAll: Boolean = false): Unit = {
    val pdfWriter: PdfWriter = new PdfWriter(path, new WriterProperties().setFullCompressionMode(true)) {
      setSmartMode(true)
    }
    val pdfDoc = new PdfDocument(pdfWriter) {
      initializeOutlines()
    }
    val doc: Document = new Document(pdfDoc, pageSize)

    val titul: PdfDocument = genTitulA4(docNameEN)
    titul.copyPagesTo(1, 1, pdfDoc)

    generatePartListPages(docNameEN, totalItems).foreach(page => {
      page.copyPagesTo(1, 1, pdfDoc)
    })

    if (genAll) {
      generatePartListPages(docNameEN, items).foreach(page => {
        page.copyPagesTo(1, 1, pdfDoc)
      })
    }

    (1 to pdfDoc.getNumberOfPages).foreach(i => {
      if (i == 1) {
        val pa = new Paragraph("1/" + pdfDoc.getNumberOfPages.toString)
        pa.setFontSize(mmToPt(5))
        pa.setFont(fontHELVETICA)
        doc.showTextAligned(pa, mmToPt(204f), mmToPt(14), i, TextAlignment.RIGHT, VerticalAlignment.TOP, 0)
      } else {
        val pa = new Paragraph(i.toString)
        pa.setFontSize(mmToPt(5))
        pa.setFont(fontHELVETICA)
        doc.showTextAligned(pa, mmToPt(200f), mmToPt(14), i, TextAlignment.RIGHT, VerticalAlignment.TOP, 0)
      }
    })

    doc.close()

  }

  private def genTitulA4(docNameEN: DocNameEN, date: String = dateNow): PdfDocument = {
    val os: OutputStream = new ByteArrayOutputStream()
    val pdfWriter = new PdfWriter(os)
    val pdfDoc = new PdfDocument(pdfWriter)
    val doc: Document = new Document(pdfDoc, pageSize) {
      setMargins(0, 0, 0, 0)
    }
    val width: Float = doc.getPdfDocument.getDefaultPageSize.getWidth
    val height: Float = doc.getPdfDocument.getDefaultPageSize.getHeight
    val logo = getNnauticLigoEN.scaleToFit(82, 30).setFixedPosition(mmToPt(23), mmToPt(10))
    doc.add(logo)
    border5mm(pdfDoc.getPage(1))
    val stamp: Table = stampEN()
    fillStamp(doc, docNameEN)
    doc.add(stamp)
    doc.close()
    os.flush()
    os.close()
    new PdfDocument(new PdfReader(new ByteArrayInputStream(os.asInstanceOf[ByteArrayOutputStream].toByteArray)))
  }

  private def generatePartListPages(docNameEN: DocNameEN, items: List[Item11ColumnsEN]): List[PdfDocument] = {
    val pages: ListBuffer[PartListBodyPage] = ListBuffer.empty[PartListBodyPage]
    pages += new PartListBodyPage(docNameEN)
    var currPage = 1
    items.foreach(row => {
      if (!pages.last.hasRoom(2)) {
        pages += new PartListBodyPage(docNameEN)
        currPage = currPage + 1
      }
      pages.last.insertRow(row)
    })
    pages.last.setLastPage()

    val retBuff = ListBuffer.empty[PdfDocument]
    pages.foreach(p => {
      p.doc.close()
      p.os.flush()
      p.os.close()
      val ba = new ByteArrayInputStream(p.os.asInstanceOf[ByteArrayOutputStream].toByteArray)
      retBuff += new PdfDocument(new PdfReader(ba))
    })
    retBuff.toList
  }


  private class PartListBodyPage(docNameEN: DocNameEN) {

    private val maxRow = 52
    private var currentRow = 0

    val os: OutputStream = new ByteArrayOutputStream()
    val pdfWriter = new PdfWriter(os)
    val pdfDoc = new PdfDocument(pdfWriter)
    val doc: Document = {
      val d = new Document(pdfDoc, pageSize)
      d.setMargins(0, 0, 0, 0)
      d
    }
    val width: Float = doc.getPdfDocument.getDefaultPageSize.getWidth
    val height: Float = doc.getPdfDocument.getDefaultPageSize.getHeight
    val logo = getNnauticLigoEN.scaleToFit(82, 30).setFixedPosition(mmToPt(23), mmToPt(10))
    doc.add(logo)

    val stamp: Table = stampEN()
    fillStamp(doc, docNameEN)

    doc.add(stamp)

    border5mm(pdfDoc.getPage(1))

    val header: Table = proceedOrderListHeader()

    val bodyGrid: Table = {
      val table = new Table(pointColumnWidths)
      val tableWidthPt = pointColumnWidths.sum
      val tableHeightPt = mmToPt(maxRow * 5)
      table.setWidth(tableWidthPt)
      table.setHeight(tableHeightPt)
      table.setFixedLayout()
      table.setHorizontalAlignment(HorizontalAlignment.CENTER)
      table.setVerticalAlignment(VerticalAlignment.MIDDLE)
      table
    }

    header.setFixedPosition(1, mmToPt(5), height - mmToPt(12), bodyGrid.getWidth)
    doc.add(header)

    bodyGrid.setFixedPosition(1, mmToPt(5), height - mmToPt(maxRow * 5) - mmToPt(12), bodyGrid.getWidth)


    def proceedOrderListHeader(debug: Boolean = true): Table = {
      val defaultFontSize = mmToPt(3.0)
      val cellBuff = ListBuffer.empty[Cell]

      val table = new Table(pointColumnWidths)
      val tableWidthPt = pointColumnWidths.sum
      val tableHeightPt = mmToPt(7)
      table.setWidth(tableWidthPt)
      table.setHeight(tableHeightPt)
      table.setHorizontalAlignment(HorizontalAlignment.CENTER)
      table.setVerticalAlignment(VerticalAlignment.MIDDLE)
      table.setFixedLayout()

      def generateRows(): Unit = {
        cellBuff += {
          new Cell(0, 0)
            .setVerticalAlignment(VerticalAlignment.MIDDLE)
            .setHorizontalAlignment(HorizontalAlignment.CENTER)
            .add(new Paragraph("")
              .setTextAlignment(TextAlignment.CENTER)
              .setFontSize(defaultFontSize)
              .setFont(fontHELVETICA)
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
              .setFont(fontHELVETICA)
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
              .setFont(fontHELVETICA)
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
              .setFont(fontHELVETICA)
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
              .setFont(fontHELVETICA)
            )
            .setPadding(0).setMargin(0)
            .setHeight(mmToPt(5))
        }
      }

      generateRows()

      cellBuff.foreach(cell => {
        table.addCell(cell)
      })

      processStaticText()

      def processStaticText(): Unit = {
        setStampText(cellBuff(0), "N", italic = false, bold = true)
        setStampText(cellBuff(1), "MATERIAL", italic = false, bold = true)
        setStampText(cellBuff(2), "UNIT", italic = false, bold = true)
        setStampText(cellBuff(3), "QTY", italic = false, bold = true)
        setStampText(cellBuff(4), "WGT, kg", italic = false, bold = true)
      }

      def setStampText(cell: Cell, text: String,
                       italic: Boolean = false,
                       bold: Boolean = false,
                       textAlignment: TextAlignment = TextAlignment.CENTER,
                       fontSize: Float = 3.0f): Cell = {
        cell.setFontSize(mmToPt(fontSize))
        val p: Paragraph = cell.getChildren.asScala.head.asInstanceOf[Paragraph]
        p.setTextAlignment(textAlignment)
        if (textAlignment == TextAlignment.CENTER) {
          cell.setPaddingLeft(mmToPt(1.5))
        }
        val t: Text = p.getChildren.asScala.head.asInstanceOf[Text]
        t.setText(text)
        if (italic) t.setItalic()
        if (bold) t.setBold()
        cell
      }

      table
    }

    def insertDummyRow(): Unit = {
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      currentRow = currentRow + 1
    }

    def generateDummyCell(): Cell = {

      new Cell(2, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph("")
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(fontHELVETICA)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def generateCellDiffSize(in: String, maxLen: Int): Cell = {
      val par = new Paragraph(in) {
        setFont(fontHELVETICA)
      }

      if (in.length >= maxLen) {
        par.setTextAlignment(TextAlignment.CENTER)
        par.setFontSize(mmToPt(3.0))
      } else {
        par.setTextAlignment(TextAlignment.CENTER)
        par.setFontSize(defaultFontSize)
        par.setFont(fontHELVETICA)
      }

      new Cell(2, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(par)
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def generateBoldCellDiffSize(in: String, maxLen: Int): Cell = {
      val par = new Paragraph(in) {
        setFont(fontHELVETICA)
      }

      if (in.length >= maxLen) {
        par.setTextAlignment(TextAlignment.CENTER)
        par.setFontSize(mmToPt(3.0))
          .setBold()
      } else {
        par.setTextAlignment(TextAlignment.CENTER)
        par.setFontSize(defaultFontSize)
        par.setFont(fontHELVETICA)
          .setBold()
      }

      new Cell(2, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(par)
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def generateCell(in: String): Cell = {
      new Cell(2, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph(in)
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(fontHELVETICA)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def generateCellLeftAlignOld(in: String): Cell = {
      new Cell(2, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)

        .add(new Paragraph(in)
          .setTextAlignment(TextAlignment.LEFT)
          .setPaddingLeft(5)
          .setFontSize(defaultFontSize)
          .setFont(fontHELVETICA)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def generateCellLeftAlign(in: String): Cell = {
      /*      val div = new Div().setMargin(0).setPadding(0).setKeepTogether(true)
            val txt = new Text(content).setFont(font).setFontSize(mmToPt(fontSizeMM))
            div.add(new Paragraph(txt))
            div.setFixedPosition(mmToPt(lMM), mmToPt(bMM), mmToPt(wMM))
            div*/



      new Cell(2, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)


        .add(new Paragraph(in)
          .setTextAlignment(TextAlignment.LEFT)
          .setPaddingLeft(5)
          .setFontSize(defaultFontSize)
          .setFont(fontHELVETICA)
        )
        .setPadding(0).setMargin(0).setWidth(10000)
        .setHeight(mmToPt(5))
    }

    def generateBoldCell(in: String): Cell = {
      new Cell(2, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph(in)
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(fontHELVETICA)
          .setBold()
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def hasRoom(rows: Int): Boolean = {
      if (currentRow + rows < maxRow) {
        true
      } else {
        doc.add(bodyGrid)
        false
      }
    }

    def setLastPage(): Unit = {
      while (currentRow != maxRow) {
        (1 to pointColumnWidths.length).foreach(i => {
          bodyGrid.addCell(generateDummyCell())
        })
        currentRow = currentRow + 1
      }
      doc.add(bodyGrid)
    }

    def generateSpannedCellBold(in: String): Cell = {
      new Cell(2, 11).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph(in)
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(fontHELVETICA)
          .setBold()
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def generateSpannedCell(in: String): Cell = {
      new Cell(2, 11).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph(in)
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(fontHELVETICA)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def insertRow(item: Item11ColumnsEN): Unit = {
      if (!item.isHeader) {
        bodyGrid.addCell(generateCellDiffSize(item.A1, 3))
        bodyGrid.addCell(generateCellLeftAlign(item.A2))
        bodyGrid.addCell(generateCell(item.A3))
        bodyGrid.addCell(generateCell(item.A4))
        bodyGrid.addCell(generateCell(item.A5))
      }
      else {
        bodyGrid.addCell(generateSpannedCellBold(item.A1))
      }
      currentRow = currentRow + 1
    }

  }

  private def genRows(rawData: List[PipeSeg]): List[Item11ColumnsEN] = {
    val rows: ListBuffer[Item11ColumnsEN] = ListBuffer.empty[Item11ColumnsEN]
    rawData.foreach(row => {
      val id = formatSpoolId(row.spool, row.spPieceId.toString)
      //val mat = row.material.code+ " "+ row.material.name
      val mat = row.material.name
      val qty = formatQTY(row)
      val unit = formatUnits(row.material)
      val weight: String = formatWGT(row)
      rows += Item11ColumnsEN(A1 = id, A2 = mat, A3 = unit, A4 = qty, A5 = weight, A11 = row.typeCode, A12 = row.material.code)
    })
    rows.sortBy(s => s.A1).toList
  }

  private def formatSpoolId(spool: String, elem: String): String = {

    elem.length match {
      case 1 => spool + ".00" + elem
      case 2 => spool + ".0" + elem
      case _ => spool + "." + elem
    }

  }

  private def formatQTY(ps: PipeSeg): String = {

    ps.material.units match {
      case "006" => {
        if (ps.typeCode.equals("PIPE")) {
          if (ps.length < 0.1) "0.1" else String.format("%.2f", ps.length / 1000.0)
        } else {
          if (ps.length < 0.1) "0.1" else String.format("%.2f", ps.length)
        }

      }
      case "796" => {
        ps.typeCode match {
          case "PIPE" => {
            if (ps.length < 0.1) "0.1" else String.format("%.1f", ps.length)
          }
          case _ =>
            if (ps.length < 1.0)
              "1"
            else
              Math.ceil(ps.length).toInt.toString
        }
      }

      case _ => String.format("%.1f", ps.length)

    }
  }

  private def formatWGT(ps: PipeSeg): String = {
    ps.typeCode match {
      case "PIPE" => String.format("%.2f", ps.weight)
      case _ => {
        val qty = if (ps.length == 0.0) 1 else ps.length
        val w = ps.material.singleWeight * qty
        if (w < 0.01) " 0.01" else String.format("%.2f", w)
      }
    }
  }

  private def formatUnits(mat: Material): String = {
    mat.units match {
      case "006" => "m"
      case "796" => "pcs"
      case _ => "NA"
    }
  }

  private def genTotal(in: List[Item11ColumnsEN]): List[Item11ColumnsEN] = {
    val buff = ListBuffer.empty[Item11ColumnsEN]
    in.groupBy(s => s.A12).foreach(gr => {
      val ent = gr._2.head
      val qty = {
        val w = gr._2.map(_.A4.toDoubleOption.getOrElse(0.0)).sum
        if (ent.A3.equals("pcs")) {
          Math.ceil(w).toInt.toString
        } else {
          if (w < 0.01) " 0.01" else String.format("%.2f", w)
        }


      }
      val wgt = {
        val w = gr._2.map(_.A5.toDoubleOption.getOrElse(0.0)).sum
        if (w < 0.01) " 0.01" else String.format("%.2f", w)
      }
      buff += Item11ColumnsEN(A1 = "", A2 = ent.A2, A3 = ent.A3, A4 = qty, A5 = wgt)
    })
    buff.sortBy(s => s.A2).toList
  }


}
