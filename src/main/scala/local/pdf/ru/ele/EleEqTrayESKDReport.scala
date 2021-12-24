package local.pdf.ru.ele

import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf.action.PdfAction
import com.itextpdf.kernel.pdf.navigation.PdfExplicitDestination
import com.itextpdf.kernel.pdf.{PdfDocument, PdfOutline, PdfReader, PdfWriter, WriterProperties}
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import local.pdf.ru.common.ReportCommon
import local.pdf.ru.common.ReportCommon.{DocName, Item11Columns, borderESKD, genBaseStampBig, genBaseStampSmall, getNnauticLigo, gostFont, mmToPt}
import org.davidmoten.text.utils.WordWrap

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters._

object EleEqTrayESKDReport {
  private val pageSize: PageSize = PageSize.A3.rotate()
  private val kM: Float = 1.437F
  private val defaultFontSize: Float = mmToPt(3.5)
  private case class DescrTree(pageNum: Int, A1: String, A2: String)
  private val descrTreeBuffer = ListBuffer.empty[DescrTree]

  private val pointColumnWidths = Array(
    mmToPt(9 * kM),
    mmToPt(12 * kM),
    mmToPt(40 * kM),
    mmToPt(119 * kM),
    mmToPt(35 * kM),
    mmToPt(10 * kM),
    mmToPt(10 * kM),//
    mmToPt(10 * kM),
    mmToPt(10 * kM),
    mmToPt(15 * kM),//
    mmToPt(10 * kM) + 2
  )

  def genReport(docName: DocName, items: List[Item11Columns], isNewRevision: Boolean = false): Unit = {
    descrTreeBuffer.clear()
    val titul: PdfDocument = genTitul(docName)
    val pdfWriter: PdfWriter = new PdfWriter(s"C:/1/${docName.num}_${docName.name}_rev${docName.lastRev}.pdf", new WriterProperties().setFullCompressionMode(true)) {
      setSmartMode(true)
    }
    val pdfDoc = new PdfDocument(pdfWriter) {
      initializeOutlines()
    }
    val doc: Document = new Document(pdfDoc, pageSize)
    titul.copyPagesTo(1, 1, pdfDoc)

    generateOrderPages("", items, docName).foreach(page => {
      page.copyPagesTo(1, 1, pdfDoc)
    })

    val root: PdfOutline = pdfDoc.getOutlines(false)
    descrTreeBuffer.foreach(descr => {
      val outline: PdfOutline = root.addOutline(descr.A1 + " " + descr.A2)
      val page = pdfDoc.getPage(descr.pageNum + 1)
      val dest: PdfExplicitDestination = PdfExplicitDestination.createFitH(page, 0f)
      val action = PdfAction.createGoTo(dest)
      outline.addAction(action)
    })

    doc.close()
  }

  private def generateWrappedRows(item: Item11Columns): List[Item11Columns] = {
    val A1count = 8
    val A2count = 12
    val A3count = 50
    val A4count = 240
    val A5count = 20
    val A6count = 7
    val A7count = 7
    val A8count = 10
    val A9count = 10
    val A10count = 26
    val A11count = 26
    val buff = ListBuffer.empty[Item11Columns]
    if (item.isHeader) {
      buff += item
    } else {
      val A1 = WordWrap.from(item.A1).newLine("$").insertHyphens(false).maxWidth(A1count).wrap().split('$')
      val A2 = WordWrap.from(item.A2).newLine("$").insertHyphens(false).maxWidth(A2count).wrap().split('$')
      val A3 = WordWrap.from(item.A3).newLine("$").insertHyphens(false).maxWidth(A3count).wrap().split('$')
      val A4 = WordWrap.from(item.A4).newLine("$").insertHyphens(false).maxWidth(A4count).wrap().split('$')
      val A5 = WordWrap.from(item.A5).newLine("$").insertHyphens(false).maxWidth(A5count).wrap().split('$')
      val A6 = WordWrap.from(item.A6).newLine("$").insertHyphens(false).maxWidth(A6count).wrap().split('$')
      val A7 = WordWrap.from(item.A7).newLine("$").insertHyphens(false).maxWidth(A7count).wrap().split('$')
      val A8 = WordWrap.from(item.A8).newLine("$").insertHyphens(false).maxWidth(A8count).wrap().split('$')
      val A9 = WordWrap.from(item.A9).newLine("$").insertHyphens(false).maxWidth(A9count).wrap().split('$')
      val A10 = WordWrap.from(item.A10).newLine("$").insertHyphens(false).maxWidth(A10count).wrap().split('$')
      val A11 = WordWrap.from(item.A11).newLine("$").insertHyphens(false).maxWidth(A11count).wrap().split('$')

      val maxRows: Int = {
        var count = 1
        if (A1.length > count) count = A1.length
        if (A2.length > count) count = A2.length
        if (A3.length > count) count = A3.length
        if (A4.length > count) count = A4.length
        if (A5.length > count) count = A5.length
        if (A6.length > count) count = A6.length
        if (A7.length > count) count = A7.length
        if (A8.length > count) count = A8.length
        if (A9.length > count) count = A9.length
        if (A10.length > count) count = A10.length
        if (A11.length > count) count = A11.length
        count
      }
      (0 until maxRows).foreach(i => {
        buff += Item11Columns(
          isHeader = false,
          A1.lift(i).getOrElse(""),
          A2.lift(i).getOrElse(""),
          A3.lift(i).getOrElse(""),
          A4.lift(i).getOrElse(""),
          A5.lift(i).getOrElse(""),
          A6.lift(i).getOrElse(""),
          A7.lift(i).getOrElse(""),
          A8.lift(i).getOrElse(""),
          A9.lift(i).getOrElse(""),
          A10.lift(i).getOrElse(""),
          A11.lift(i).getOrElse("")
        )
      })
    }
    buff.toList
  }


  private def genTitul(docName: DocName): PdfDocument = {
    val os: OutputStream = new ByteArrayOutputStream()
    val pdfWriter = new PdfWriter(os)
    val pdfDoc = new PdfDocument(pdfWriter)
    val doc: Document = new Document(pdfDoc, pageSize) {
      setMargins(0, 0, 0, 0)
    }
    val width: Float = doc.getPdfDocument.getDefaultPageSize.getWidth
    val height: Float = doc.getPdfDocument.getDefaultPageSize.getHeight
    val bigStamp: Table = genBaseStampBig(docName)
    bigStamp.setFixedPosition(1, width - bigStamp.getWidth.getValue - mmToPt(5) - 2, mmToPt(5), bigStamp.getWidth.getValue)
    doc.add(bigStamp)
    borderESKD(pdfDoc.getPage(1))
    val logo = getNnauticLigo
      .scaleToFit(82, 30)
      .setFixedPosition(mmToPt(242), mmToPt(6.5))
    doc.add(logo)
    doc.close()
    os.flush()
    os.close()
    new PdfDocument(new PdfReader(new ByteArrayInputStream(os.asInstanceOf[ByteArrayOutputStream].toByteArray)))
  }

  private def generateOrderPages(header: String, orderRows: List[Item11Columns], docName: DocName): List[PdfDocument] = {
    val pages: ListBuffer[OrderBodyPage] = ListBuffer.empty[OrderBodyPage]
    pages += new OrderBodyPage(header, docName)
    if (header.nonEmpty) pages.last.insertRow(List[Item11Columns](Item11Columns(isHeader = true, header)))

    var currPage = 1

    orderRows.foreach(row => {

      val wrappedRows: List[Item11Columns] = generateWrappedRows(row)
      if (!pages.last.hasRoom(wrappedRows.length + 2)) {
        pages += new OrderBodyPage(header, docName)
        currPage = currPage + 1
      }
      pages.last.insertRow(wrappedRows)
      //pages.last.insertDummyRow()
      if (row.isHeader) {
        descrTreeBuffer += DescrTree(currPage, row.A1, row.A2)
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

  private class OrderBodyPage(addName: String, docName: DocName) {
    val docNameLocal: DocName = {
      if (addName.nonEmpty) {
        DocName(docName.code, docName.num + " " + addName, docName.name, docName.lastRev, docName.userDev, docName.userTCheck, docName.userNCheck, docName.userAgree)
      } else {
        docName
      }
    }

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
    val baseStampSmall: Table = genBaseStampSmall(docNameLocal)
    baseStampSmall.setFixedPosition(1, width - baseStampSmall.getWidth.getValue - mmToPt(5) - 2, mmToPt(5), baseStampSmall.getWidth.getValue)
    doc.add(baseStampSmall)
    borderESKD(pdfDoc.getPage(1))

    val header: Table = proceedOrderListHeader()
    header.setFixedPosition(1, mmToPt(20), height - mmToPt(20), header.getWidth.getValue)
    doc.add(header)

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
    bodyGrid.setFixedPosition(1, mmToPt(20), height - mmToPt(maxRow * 5) - mmToPt(20), bodyGrid.getWidth)


    def proceedOrderListHeader(debug: Boolean = true): Table = {
      val defaultFontSize = mmToPt(4.1)
      val cellBuff = ListBuffer.empty[Cell]

      val table = new Table(pointColumnWidths)
      val k = pointColumnWidths.sum
      val tableWidthPt = pointColumnWidths.sum
      val tableHeightPt = mmToPt(15)
      table.setWidth(tableWidthPt)
      table.setHeight(tableHeightPt)
      table.setHorizontalAlignment(HorizontalAlignment.CENTER)
      table.setVerticalAlignment(VerticalAlignment.MIDDLE)
      table.setFixedLayout()

      def generateRows(): Unit = {
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
            .setHeight(mmToPt(5))
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
            .setHeight(mmToPt(5))
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
            .setHeight(mmToPt(5))
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
            .setHeight(mmToPt(5))
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
            .setHeight(mmToPt(5))
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
            .setHeight(mmToPt(5))
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
            .setHeight(mmToPt(5))
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
            .setHeight(mmToPt(5))
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
            .setHeight(mmToPt(5))
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
      }

      generateRows()
      cellBuff.foreach(cell => {
        if (debug)
          cell.getChildren.asScala.head.asInstanceOf[Paragraph].getChildren.asScala.head.asInstanceOf[Text]
            .setText(cellBuff.indexOf(cell).toString)
        table.addCell(cell)
      })
      processStaticText()

      def processStaticText(): Unit = {
        setStampText(cellBuff(0), "Поз.", italic = true)
        setStampText(cellBuff(1), "Индекс", italic = true)
        setStampText(cellBuff(2), "Обозначение", italic = true)
        setStampText(cellBuff(3), "Наименование", italic = true)
        setStampText(cellBuff(4), "Код изделия", italic = true)
        setStampText(cellBuff(5), "КЕИ", italic = true)
        setStampText(cellBuff(6), "Кол-во", italic = true)
        setStampText(cellBuff(7), "Масса", italic = true)
        setStampText(cellBuff(8), "Вед.", italic = true)
        setStampText(cellBuff(9), "Пом.", italic = true)
        setStampText(cellBuff(10), "един.", italic = true)
        setStampText(cellBuff(11), "общая", italic = true)
        setStampText(cellBuff(12), "1", italic = true)
        setStampText(cellBuff(13), "2", italic = true)
        setStampText(cellBuff(14), "3", italic = true)
        setStampText(cellBuff(15), "4", italic = true)
        setStampText(cellBuff(16), "5", italic = true)
        setStampText(cellBuff(17), "6", italic = true)
        setStampText(cellBuff(18), "7", italic = true)
        setStampText(cellBuff(19), "8", italic = true)
        setStampText(cellBuff(20), "9", italic = true)
        setStampText(cellBuff(21), "10", italic = true)
        setStampText(cellBuff(22), "11", italic = true)
      }

      def setStampText(cell: Cell, text: String, italic: Boolean = false, bold: Boolean = false, textAlignment: TextAlignment = TextAlignment.CENTER, fontSize: Float = 4.1f): Cell = {

        cell.setFontSize(mmToPt(fontSize))
        val p: Paragraph = cell.getChildren.asScala.head.asInstanceOf[Paragraph]
        p.setTextAlignment(textAlignment)

        if (textAlignment == TextAlignment.LEFT) {
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
      bodyGrid.addCell(generateCell(""))
      currentRow = currentRow + 1
    }

    def generateDummyCell(): Cell = {
      new Cell(2, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph("")
          .setTextAlignment(TextAlignment.CENTER)
          .setFontSize(defaultFontSize)
          .setFont(gostFont)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def generateCellDiffSize(in: String, maxLen: Int): Cell = {
      val par = new Paragraph(in) {
        setFont(gostFont)
      }

      if (in.length >= maxLen) {
        par.setTextAlignment(TextAlignment.CENTER)
        par.setFontSize(mmToPt(3.0))
      } else {
        par.setTextAlignment(TextAlignment.CENTER)
        par.setFontSize(defaultFontSize)
        par.setFont(gostFont)
      }

      new Cell(2, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(par)
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def generateBoldCellDiffSize(in: String, maxLen: Int): Cell = {
      val par = new Paragraph(in) {
        setFont(gostFont)
      }

      if (in.length >= maxLen) {
        par.setTextAlignment(TextAlignment.CENTER)
        par.setFontSize(mmToPt(3.0))
          .setBold()
      } else {
        par.setTextAlignment(TextAlignment.CENTER)
        par.setFontSize(defaultFontSize)
        par.setFont(gostFont)
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
          .setFont(gostFont)
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
          .setFont(gostFont)
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
        (1 to 11).foreach(i => {
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
          .setFont(gostFont)
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
          .setFont(gostFont)
        )
        .setPadding(0).setMargin(0)
        .setHeight(mmToPt(5))
    }

    def insertRoOld(wrappedRows: List[Item11Columns]): Unit = {
      wrappedRows.foreach(item => {
        if (item.isHeader) {
          bodyGrid.addCell(generateSpannedCellBold(item.A1))
        }
        else {
          bodyGrid.addCell(generateCellDiffSize(item.A1, 6))
          bodyGrid.addCell(generateCell(item.A2))
          bodyGrid.addCell(generateCell(item.A3))
          bodyGrid.addCell(generateCell(item.A4))
          bodyGrid.addCell(generateCell(item.A5))
          bodyGrid.addCell(generateCell(item.A6))
          bodyGrid.addCell(generateCell(item.A7))
          bodyGrid.addCell(generateCell(item.A8))
          bodyGrid.addCell(generateCell(item.A9))
          bodyGrid.addCell(generateCell(item.A10))
          bodyGrid.addCell(generateCell(item.A11))

        }
        currentRow = currentRow + 1
      })
    }

    def insertRow(wrappedRows: List[Item11Columns]): Unit = {
      wrappedRows.foreach(item => {
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
          bodyGrid.addCell(generateCell(item.A10))
          bodyGrid.addCell(generateCell(item.A11))
        }
        else {
          bodyGrid.addCell(generateSpannedCellBold(item.A1))
        }
        currentRow = currentRow + 1
      })
    }
  }


}