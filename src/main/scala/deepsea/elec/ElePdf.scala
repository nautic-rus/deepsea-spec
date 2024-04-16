package deepsea.elec

import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf.{PdfDocument, PdfReader, PdfWriter, WriterProperties}
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Div, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import deepsea.elec.ElecManager.EleElement
import deepsea.esp.EspManager.EleEspObject
import local.common.DBRequests.findChess
import local.domain.CommonTypes
import local.hull.PartManager.{PrdPart, genForanPartsByDrawingNum}
import local.pdf.UtilsPDF
import local.pdf.en.accom.AccomReportEn.dateNow
import local.pdf.en.common.ReportCommonEN
import local.pdf.en.common.ReportCommonEN.{DocNameEN, Item11ColumnsEN, addZeros, border5mm, defaultFontSize, fillStamp, fillStampH, fontHELVETICA, getNnauticLigoEN, stampEN, stampENH}
import local.pdf.ru.ele.EleEqTrayESKDReport.{findChessPos, pageSize}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.nio.file.Files
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait ElePdf extends UtilsPDF {
  case class PdfElems(partList: List[PdfElemPartList], summary: List[PdfElemSummary])
  case class PdfElemPartList(label: String, title: String, descr: String, units: String, qty: Double, wgt: Double, tWgt: Double, room: String, place: String)
  case class PdfElemSummary(label: String, title: String, descr: String, units: String, qty: Double, wgt: Double, tWgt: Double, stmt: String, matCode: String)


  private val pageSize: PageSize = PageSize.A4.rotate()
  private val pointColumnWidths = Array(
    mmToPt(20),
    mmToPt(94),
    mmToPt(60),
    mmToPt(20),
    mmToPt(20),
    mmToPt(20),
    mmToPt(20),
    mmToPt(20),
    mmToPt(20),
  )

  private val pointColumnWidthsSummary = Array(
    mmToPt(20),
    mmToPt(94),
    mmToPt(60),
    mmToPt(20),
    mmToPt(20),
    mmToPt(20),
    mmToPt(20),
    mmToPt(39),
  )

  def genElePdf(ele: EleEspObject, docName: String): String = {
    val path = Files.createTempDirectory("hullPdf").toAbsolutePath.toString + "/" + ele.docNumber + "_rev" + ele.rev + ".pdf"
    val chess: CommonTypes.DrawingChess = {
      val l = findChess(ele.docNumber, ele.rev)
      if (l.nonEmpty) {
        l.head
      } else {
        CommonTypes.DrawingChess()
      }
    }

    val dn: DocNameEN = DocNameEN(num = ele.docNumber, name = docName)
    val elems = ele.elements
    val count = ListBuffer.empty[String]
    val pdfElem = PdfElems(
      elems.map(e => PdfElemPartList(e.userId, e.material.name, e.material.description, e.units, 1, e.weight, e.weight, e.zone, "")),
      elems.groupBy(_.material.code).map(gr => {
        count += gr._1
        val label = (("0" * 10) + count.length.toString).takeRight(4)
        val m = gr._2.head.material
        val qty = gr._2.length
        val wgt = gr._2.map(_.weight).sum
        PdfElemSummary(
          label,
          m.name,
          m.description,
          m.units,
          qty,
          m.singleWeight,
          wgt,
          m.category,
          m.code
        )
      }).toList
    )
    processPDF(dn, path, pdfElem)
    path
  }

  private def processPDF(docNameEN: DocNameEN, path: String, items: PdfElems): Unit = {

    val pdfWriter: PdfWriter = new PdfWriter(path, new WriterProperties().setFullCompressionMode(true)) {
      setSmartMode(true)
    }
    val pdfDoc = new PdfDocument(pdfWriter) {
      initializeOutlines()
    }
    val doc: Document = new Document(pdfDoc, pageSize)

    val titul: PdfDocument = genTitulA4(docNameEN)

    titul.copyPagesTo(1, 1, pdfDoc)

    generatePartListPages(docNameEN, items.partList).foreach(page => {
      page.copyPagesTo(1, 1, pdfDoc)
    })

    generatePartListPagesSummary(docNameEN, items.summary).foreach(page => {
      page.copyPagesTo(1, 1, pdfDoc)
    })


    (1 to pdfDoc.getNumberOfPages).foreach(i => {
      if (i == 1) {
        val pa = new Paragraph("1/" + pdfDoc.getNumberOfPages.toString)
        pa.setFontSize(mmToPt(5))
        pa.setFont(fontHELVETICA)
        doc.showTextAligned(pa, mmToPt(290f), mmToPt(12), i, TextAlignment.RIGHT, VerticalAlignment.TOP, 0)
      } else {
        val pa = new Paragraph(i.toString)
        pa.setFontSize(mmToPt(5))
        pa.setFont(fontHELVETICA)
        doc.showTextAligned(pa, mmToPt(288f), mmToPt(12), i, TextAlignment.RIGHT, VerticalAlignment.TOP, 0)
      }
    })


    doc.close()
  }

  private def genTitulA4(docNameEN: DocNameEN): PdfDocument = {
    val os: OutputStream = new ByteArrayOutputStream()
    val pdfWriter = new PdfWriter(os)
    val pdfDoc = new PdfDocument(pdfWriter)
    val doc: Document = new Document(pdfDoc, pageSize) {
      setMargins(0, 0, 0, 0)
    }
    val logo = getNnauticLigoEN.scaleToFit(72, 20).setFixedPosition(mmToPt(96), mmToPt(8))
    doc.add(logo)
    border5mm(pdfDoc.getPage(1))
    val stamp: Table = stampENH()
    fillStampH(doc, docNameEN)
    doc.add(stamp)
    doc.close()
    os.flush()
    os.close()
    new PdfDocument(new PdfReader(new ByteArrayInputStream(os.asInstanceOf[ByteArrayOutputStream].toByteArray)))
  }

  private def generatePartListPages(docNameEN: DocNameEN, items: List[PdfElemPartList]): List[PdfDocument] = {
    val pages: ListBuffer[PartListBodyPage] = ListBuffer.empty[PartListBodyPage]
    pages += new PartListBodyPage(docNameEN)
    var currPage = 1
    items.foreach(row => {
      val rowLimit = 55
      if (row.title.length > rowLimit){
        val split = row.title.split(' ')
        var firstRow = ""
        var secondRow = ""
        for (x <- 1.to(split.length)){
          val splitRow = split.take(x).mkString(" ")
          if (splitRow.length < rowLimit){
            firstRow = splitRow
            secondRow = row.title.replace(firstRow, "")
          }
        }


        if (!pages.last.hasRoom(2)) {
          pages += new PartListBodyPage(docNameEN)
          currPage = currPage + 1
        }
        pages.last.insertRow(row.copy(title = firstRow))
        if (!pages.last.hasRoom(2)) {
          pages += new PartListBodyPage(docNameEN)
          currPage = currPage + 1
        }
        pages.last.insertTitleRow(secondRow)
      }
      else{
        if (!pages.last.hasRoom(2)) {
          pages += new PartListBodyPage(docNameEN)
          currPage = currPage + 1
        }
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
  private def generatePartListPagesSummary(docNameEN: DocNameEN, items: List[PdfElemSummary]): List[PdfDocument] = {
    val pages: ListBuffer[PartListBodyPageSummary] = ListBuffer.empty[PartListBodyPageSummary]
    pages += new PartListBodyPageSummary(docNameEN)
    var currPage = 1
    items.foreach(row => {
      val rowLimit = 55
      if (row.title.length > rowLimit){
        val split = row.title.split(' ')
        var firstRow = ""
        var secondRow = ""
        for (x <- 1.to(split.length)){
          val splitRow = split.take(x).mkString(" ")
          if (splitRow.length < rowLimit){
            firstRow = splitRow
            secondRow = row.title.replace(firstRow, "")
          }
        }


        if (!pages.last.hasRoom(2)) {
          pages += new PartListBodyPageSummary(docNameEN)
          currPage = currPage + 1
        }
        pages.last.insertRowSummary(row.copy(title = firstRow))
        if (!pages.last.hasRoom(2)) {
          pages += new PartListBodyPageSummary(docNameEN)
          currPage = currPage + 1
        }
        pages.last.insertTitleRowSummary(secondRow)
      }
      else{
        if (!pages.last.hasRoom(2)) {
          pages += new PartListBodyPageSummary(docNameEN)
          currPage = currPage + 1
        }
        pages.last.insertRowSummary(row)
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

    private val maxRow = 32
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
    val logo = getNnauticLigoEN.scaleToFit(72, 20).setFixedPosition(mmToPt(96), mmToPt(8))
    doc.add(logo)

    val stamp: Table = stampENH()
    fillStampH(doc, docNameEN)

    doc.add(stamp)

    border5mm(pdfDoc.getPage(1))

    val pHeader: Table = pageHeader()

    val header: Table = header("PART LIST | СПЕЦИФИКАЦИЯ")

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

    pHeader.setFixedPosition(1, mmToPt(5), height - mmToPt(18), bodyGrid.getWidth)
    doc.add(pHeader)

    bodyGrid.setFixedPosition(1, mmToPt(5), height - mmToPt(maxRow * 5) - mmToPt(18), bodyGrid.getWidth)


    def pageHeader(): Table = {
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
      }

      generateRows()

      cellBuff.foreach(cell => {
        table.addCell(cell)
      })

      processStaticText()

      def processStaticText(): Unit = {
        setStampText(cellBuff(0), "LABEL / ПОЗ.", italic = false, bold = true)
        setStampText(cellBuff(1), "TITLE / НАИМЕНОВАНИЕ", italic = false, bold = true)
        setStampText(cellBuff(2), "DESCRIPTION / ОБОЗНАЧЕНИЕ", italic = false, bold = true)
        setStampText(cellBuff(3), "UNITS / КЕИ", italic = false, bold = true)
        setStampText(cellBuff(4), "QTY / К-ВО", italic = false, bold = true)
        setStampText(cellBuff(5), "WGT / ВЕС", italic = false, bold = true)
        setStampText(cellBuff(6), "T.WGT / О.ВЕС", italic = false, bold = true)
        setStampText(cellBuff(7), "ROOM / ПОМ.", italic = false, bold = true)
        setStampText(cellBuff(8), "PLACE / АДРЕС", italic = false, bold = true)
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
    def header(text: String): Table = {
      val defaultFontSize = mmToPt(3.2)
      val cellBuff = ListBuffer.empty[Cell]

      val table = new Table(1)
      val tableWidthPt = pointColumnWidths.sum
      val tableHeightPt = mmToPt(7)
      table.setWidth(tableWidthPt)
      table.setHeight(tableHeightPt)
      table.setHorizontalAlignment(HorizontalAlignment.CENTER)
      table.setVerticalAlignment(VerticalAlignment.MIDDLE)
      table.setFixedLayout()

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

      cellBuff.foreach(cell => {
        table.addCell(cell)
      })
      setStampText(cellBuff.head, text, italic = false, bold = true)

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
        (1.to(pointColumnWidths.length)).foreach(i => {
          bodyGrid.addCell(generateDummyCell())
        })
        currentRow = currentRow + 1
      }
      doc.add(bodyGrid)
    }
    def setLastPageSummary(): Unit = {
      while (currentRow != maxRow) {
        (1.to(pointColumnWidthsSummary.length)).foreach(i => {
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

    def insertRow(item: PdfElemPartList): Unit = {
      bodyGrid.addCell(generateCell(item.label))
      bodyGrid.addCell(generateCell(item.title))
      bodyGrid.addCell(generateCell(item.descr))
      bodyGrid.addCell(generateCell(item.units))
      bodyGrid.addCell(generateCell((Math.round(item.qty * 100) / 100).toString))
      bodyGrid.addCell(generateCell((Math.round(item.wgt * 100) / 100).toString))
      bodyGrid.addCell(generateCell((Math.round(item.tWgt * 100) / 100).toString))
      bodyGrid.addCell(generateCell(item.room))
      bodyGrid.addCell(generateCell(item.place))
      currentRow = currentRow + 1
    }
    def insertRow(item: PdfElemSummary): Unit = {
      bodyGrid.addCell(generateCell(item.label))
      bodyGrid.addCell(generateCell(item.title))
      bodyGrid.addCell(generateCell(item.descr))
      bodyGrid.addCell(generateCell(item.units))
      bodyGrid.addCell(generateCell((Math.round(item.qty * 100) / 100).toString))
      bodyGrid.addCell(generateCell((Math.round(item.tWgt * 100) / 100).toString))
      bodyGrid.addCell(generateCell(item.stmt))
      bodyGrid.addCell(generateCell(item.matCode))
      currentRow = currentRow + 1
    }
    def insertTitleRow(title: String): Unit = {
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(title))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      currentRow = currentRow + 1
    }

  }

  private class PartListBodyPageSummary(docNameEN: DocNameEN) {

    private val maxRow = 32
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
    val logo = getNnauticLigoEN.scaleToFit(72, 20).setFixedPosition(mmToPt(96), mmToPt(8))
    doc.add(logo)

    val stamp: Table = stampENH()
    fillStampH(doc, docNameEN)

    doc.add(stamp)

    border5mm(pdfDoc.getPage(1))

    val pHeader: Table = pageHeaderSummary()

    val header: Table = headerSummary("BILLING OF MATERIALS | ЗАКАЗ МАТЕРИАЛОВ")

    val bodyGrid: Table = {
      val table = new Table(pointColumnWidthsSummary)
      val tableWidthPt = pointColumnWidthsSummary.sum
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

    pHeader.setFixedPosition(1, mmToPt(5), height - mmToPt(18), bodyGrid.getWidth)
    doc.add(pHeader)

    bodyGrid.setFixedPosition(1, mmToPt(5), height - mmToPt(maxRow * 5) - mmToPt(18), bodyGrid.getWidth)

    def pageHeaderSummary(): Table = {
      val defaultFontSize = mmToPt(3.0)
      val cellBuff = ListBuffer.empty[Cell]

      val table = new Table(pointColumnWidthsSummary)
      val tableWidthPt = pointColumnWidthsSummary.sum
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
      }

      generateRows()

      cellBuff.foreach(cell => {
        table.addCell(cell)
      })

      processStaticText()

      def processStaticText(): Unit = {
        setStampText(cellBuff(0), "LABEL / ПОЗ.", italic = false, bold = true)
        setStampText(cellBuff(1), "TITLE / НАИМЕНОВАНИЕ", italic = false, bold = true)
        setStampText(cellBuff(2), "DESCRIPTION / ОБОЗНАЧЕНИЕ", italic = false, bold = true)
        setStampText(cellBuff(3), "UNITS / ЕД.", italic = false, bold = true)
        setStampText(cellBuff(4), "QTY / К-ВО", italic = false, bold = true)
        setStampText(cellBuff(5), "WGT / ВЕС", italic = false, bold = true)
        setStampText(cellBuff(6), "ST.CODE / К.ВЕД.", italic = false, bold = true)
        setStampText(cellBuff(7), "MAT.CODE / КОД МАТ.", italic = false, bold = true)
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
    def headerSummary(text: String): Table = {
      val defaultFontSize = mmToPt(3.2)
      val cellBuff = ListBuffer.empty[Cell]

      val table = new Table(1)
      val tableWidthPt = pointColumnWidthsSummary.sum
      val tableHeightPt = mmToPt(7)
      table.setWidth(tableWidthPt)
      table.setHeight(tableHeightPt)
      table.setHorizontalAlignment(HorizontalAlignment.CENTER)
      table.setVerticalAlignment(VerticalAlignment.MIDDLE)
      table.setFixedLayout()

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

      cellBuff.foreach(cell => {
        table.addCell(cell)
      })
      setStampText(cellBuff.head, text, italic = false, bold = true)

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
        (1.to(pointColumnWidthsSummary.length)).foreach(i => {
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

    def insertRow(item: PdfElemPartList): Unit = {
      bodyGrid.addCell(generateCell(item.label))
      bodyGrid.addCell(generateCell(item.title))
      bodyGrid.addCell(generateCell(item.descr))
      bodyGrid.addCell(generateCell(item.units))
      bodyGrid.addCell(generateCell((Math.round(item.qty * 100) / 100).toString))
      bodyGrid.addCell(generateCell((Math.round(item.wgt * 100) / 100).toString))
      bodyGrid.addCell(generateCell((Math.round(item.tWgt * 100) / 100).toString))
      bodyGrid.addCell(generateCell(item.room))
      bodyGrid.addCell(generateCell(item.place))
      currentRow = currentRow + 1
    }
    def insertRow(item: PdfElemSummary): Unit = {
      bodyGrid.addCell(generateCell(item.label))
      bodyGrid.addCell(generateCell(item.title))
      bodyGrid.addCell(generateCell(item.descr))
      bodyGrid.addCell(generateCell(item.units))
      bodyGrid.addCell(generateCell((Math.round(item.qty * 100) / 100).toString))
      bodyGrid.addCell(generateCell((Math.round(item.tWgt * 100) / 100).toString))
      bodyGrid.addCell(generateCell(item.stmt))
      bodyGrid.addCell(generateCell(item.matCode))
      currentRow = currentRow + 1
    }
    def insertRowSummary(item: PdfElemSummary): Unit = {
      bodyGrid.addCell(generateCell(item.label))
      bodyGrid.addCell(generateCell(item.title))
      bodyGrid.addCell(generateCell(item.descr))
      bodyGrid.addCell(generateCell(item.units))
      bodyGrid.addCell(generateCell((Math.round(item.qty * 100) / 100).toString))
      bodyGrid.addCell(generateCell((Math.round(item.tWgt * 100) / 100).toString))
      bodyGrid.addCell(generateCell(item.stmt))
      bodyGrid.addCell(generateCell(item.matCode))
      currentRow = currentRow + 1
    }
    def insertTitleRow(title: String): Unit = {
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(title))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      currentRow = currentRow + 1
    }
    def insertTitleRowSummary(title: String): Unit = {
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(title))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      bodyGrid.addCell(generateCell(""))
      currentRow = currentRow + 1
    }
  }


}
