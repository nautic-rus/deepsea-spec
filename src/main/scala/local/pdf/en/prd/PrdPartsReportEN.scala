package local.pdf.en.prd

import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf.{PdfDocument, PdfReader, PdfWriter, WriterProperties}
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Div, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import local.common.DBRequests.findChess
import local.domain.CommonTypes
import local.hull.PartManager.{PrdPart, genForanPartsByDrawingNum}
import local.pdf.UtilsPDF
import local.pdf.en.common.ReportCommonEN
import local.pdf.en.common.ReportCommonEN.{DocNameEN, Item11ColumnsEN, border5mm, defaultFontSize, fillStamp, fontHELVETICA, getNnauticLigoEN, stampEN}
import local.pdf.ru.common.ReportCommon.mmToPt
import local.pdf.ru.ele.EleEqTrayESKDReport.{findChessPos, pageSize}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala

object PrdPartsReportEN extends UtilsPDF {
  private val pageSize: PageSize = PageSize.A4
//  private val pointColumnWidths = Array(
//    mmToPt(10),
//    mmToPt(10),
//    mmToPt(10),
//    mmToPt(26),
//    mmToPt(15),
//
//    mmToPt(10),
//    mmToPt(15),
//    mmToPt(15),
//    mmToPt(81),
//
//
//    mmToPt(15) + 2 //
//  )
  private val pointColumnWidths = Array(
    mmToPt(10),
    mmToPt(10),
    mmToPt(10),
    mmToPt(71),
    mmToPt(20),

    mmToPt(10),
    mmToPt(15),
    mmToPt(15),
    mmToPt(31),


    mmToPt(15) + 2 //
  )

  def genHullPartListEnPDF(project: String, docNumber: String, docName: String, revision: String, path: String, additional: List[PrdPart]): Unit = {
    val partsInit: List[PrdPart] = genForanPartsByDrawingNum(project, docNumber)
    val parts: List[PrdPart] =
      if ((project == "N008" || project == "N004") && docNumber.contains("240402-210") ) {
        partsInit.filter(element => element.PART_CODE.toIntOption.exists(_ > 2000))
      } else {
        partsInit
      }
    val chess: CommonTypes.DrawingChess = {
      val l = findChess(docNumber, revision)
      if (l.nonEmpty) {
        l.head
      } else {
        CommonTypes.DrawingChess()
      }

    }
    val rows: List[Item11ColumnsEN] = {
      val buff: ListBuffer[Item11ColumnsEN] = ListBuffer.empty[Item11ColumnsEN]
      parts.groupBy(s => (s.PART_CODE, s.SYMMETRY)).toList.foreach(gr => {
        val qty = gr._2.length
        val nestids: String = {
          val buff = ListBuffer.empty[String]
          gr._2.foreach(ent => buff += ent.NEST_ID)
          buff.toList.distinct.mkString(";")
        }
        val id = gr._2.head.PART_CODE
        val weight = String.format("%.2f", gr._2.head.WEIGHT_UNIT)
        val totWeight = String.format("%.2f", gr._2.head.WEIGHT_UNIT * qty)
        val symm = gr._2.head.SYMMETRY
        val elemType: String = {
          val v = gr._2.head.ELEM_TYPE.toUpperCase
          if (v.equals("FS")) "FB" else if (v.equals("BS")) "HP" else
            v
        }
        val mat = gr._2.head.MATERIAL
        val kpl_kse = {
          elemType match {
            case "PL" => "S" + String.format("%.1f", gr._2.head.THICKNESS)
            case "FS" => "S" + String.format("%.1f", gr._2.head.THICKNESS)
            case _ => String.format("%.1f", gr._2.head.WIDTH) + "x" + String.format("%.1f", gr._2.head.THICKNESS)
          }
        }
        //buff += Item11ColumnsEN(A1 = id, A2 = symm, A3 = elemType, A4 = kpl_kse, A5 = mat, A6 = qty.toString, A7 = weight, A8 = totWeight, A9 = nestids, A12 = findChessPos(id + "-" + symm, chess))
        buff += Item11ColumnsEN(A1 = id, A2 = symm, A3 = elemType, A4 = kpl_kse, A5 = mat, A6 = qty.toString, A7 = weight, A8 = totWeight, A12 = findChessPos(id + "-" + symm, chess))
      })
      additional.foreach(part => {
        buff += (Item11ColumnsEN(
          false, part.PART_CODE, "", "M", part.DESCRIPTION, "", part.QTY.toString, part.WEIGHT_UNIT.toString, (Math.round(part.TOTAL_WEIGHT * 1000) / 1000d).toString, part.NEST_ID, "", "", "", "")
        )
      })
      buff.sortBy(s => s.A1).toList
    }

    val dn: DocNameEN = {
      if (revision != "") {
        DocNameEN(num = docNumber, name = docName, lastRev = revision)
      }
      else {
        DocNameEN(num = docNumber, name = docName)
      }
    }


    processPDF(dn, path, rows, department = "hull")
  }

  private def processPDF(docNameEN: DocNameEN, path: String, items: List[Item11ColumnsEN], department: String): Unit = {

    val pdfWriter: PdfWriter = new PdfWriter(path, new WriterProperties().setFullCompressionMode(true)) {
      setSmartMode(true)
    }
    val pdfDoc = new PdfDocument(pdfWriter) {
      initializeOutlines()
    }
    val doc: Document = new Document(pdfDoc, pageSize)

    val titul: PdfDocument = genTitulA4(docNameEN, department)

    titul.copyPagesTo(1, 1, pdfDoc)

    generatePartListPages(docNameEN, items).foreach(page => {
      page.copyPagesTo(1, 1, pdfDoc)
    })


    (1 to pdfDoc.getNumberOfPages).foreach(i => {
      if (i == 1) {
        val pa = new Paragraph("1/" + pdfDoc.getNumberOfPages.toString)
        pa.setFontSize(mmToPt(5))
        pa.setFont(fontHELVETICA)
        doc.showTextAligned(pa, mmToPt(203f), mmToPt(12.5), i, TextAlignment.RIGHT, VerticalAlignment.TOP, 0)
      } else {
        val pa = new Paragraph(i.toString)
        pa.setFontSize(mmToPt(5))
        pa.setFont(fontHELVETICA)
        doc.showTextAligned(pa, mmToPt(201f), mmToPt(12.5), i, TextAlignment.RIGHT, VerticalAlignment.TOP, 0)
      }
    })


    doc.close()
  }

  private def genTitulA4(docNameEN: DocNameEN, date: String = dateNow, department: String = ""): PdfDocument = {
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
    val stamp: Table = stampEN()
    fillStamp(doc, docNameEN, department)
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
      val rowA4Limit = 35
      val rowA9Limit = 16
      if (row.A4.length > rowA4Limit || row.A9.length > rowA9Limit){
        val splitA4 = ListBuffer.empty[String]
        val splitA9 = ListBuffer.empty[String]

        val splitA4BySpace = row.A4.split(" ")
        val splitA9BySpace = row.A9.split(" ")


        val splits = ListBuffer.empty[String]
        var rowSplit = ""
        var error = false
        splitA4BySpace.foreach(s => {
          if ((s + rowSplit).length > rowA4Limit && rowSplit != ""){
            splits += rowSplit
            rowSplit = s
          }
          else if ((s + rowSplit).length > rowA4Limit && rowSplit == ""){
            error = true
          }
          else{
            rowSplit = (rowSplit + " " + s).trim
          }
        })
        splits += rowSplit
        if (!error){
          splitA4 ++= splits
        }

        splits.clear()
        rowSplit = ""
        error = false
        splitA9BySpace.foreach(s => {
          if ((s + rowSplit).length > rowA9Limit && rowSplit != ""){
            splits += rowSplit
            rowSplit = s
          }
          else if ((s + rowSplit).length > rowA9Limit && rowSplit == ""){
            error = true
          }
          else{
            rowSplit = (rowSplit + " " + s).trim
          }
        })
        splits += rowSplit
        if (!error){
          splitA9 ++= splits
        }

        if (splitA4.isEmpty){
          splitA4 ++= row.A4.grouped(rowA4Limit).toList
        }
        if (splitA9.isEmpty){
          splitA9 ++= row.A9.grouped(rowA9Limit).toList
        }

        val groupLength = List(splitA4.length, splitA9.length).max

        pages.last.insertRow(row.copy(A4 = splitA4.head, A9 = splitA9.head))
        for (x <- 1.until(groupLength)){
          pages.last.insertRow(Item11ColumnsEN(false, "", A4 = if (x > splitA4.length - 1) "" else splitA4(x), A9 = if (x > splitA9.length - 1) "" else splitA9(x)))
        }
      }
      else{
        pages.last.insertRow(row)
      }
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
    val logo = getNnauticLigoEN.scaleToFit(72, 20).setFixedPosition(mmToPt(9), mmToPt(8))
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
        setStampText(cellBuff(0), "ID", italic = false, bold = true)
        setStampText(cellBuff(1), "SYMM", italic = false, bold = true)
        setStampText(cellBuff(2), "TYPE", italic = false, bold = true)
        setStampText(cellBuff(3), "DIM", italic = false, bold = true)
        setStampText(cellBuff(4), "MAT", italic = false, bold = true)
        setStampText(cellBuff(5), "QTY", italic = false, bold = true)

        setStampText(cellBuff(6), "W(kg)", italic = false, bold = true)
        setStampText(cellBuff(7), "W Tot(kg)", italic = false, bold = true)
        setStampText(cellBuff(8), "NOTE", italic = false, bold = true)
        setStampText(cellBuff(9), "DR. PLACE", italic = false, bold = true)
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
        (1 to 10).foreach(i => {
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
        bodyGrid.addCell(generateCell(item.A2))
        bodyGrid.addCell(generateCell(item.A3))
        bodyGrid.addCell(generateCell(item.A4))
        bodyGrid.addCell(generateCell(item.A5))
        bodyGrid.addCell(generateCell(item.A6))

        bodyGrid.addCell(generateCell(item.A7))
        bodyGrid.addCell(generateCell(item.A8))
        bodyGrid.addCell(generateCell(item.A9))
        //bodyGrid.addCell(generateCell(""))
        bodyGrid.addCell(generateCell(item.A12))
      }
      else {
        bodyGrid.addCell(generateSpannedCellBold(item.A1))
      }
      currentRow = currentRow + 1
    }

  }

}
