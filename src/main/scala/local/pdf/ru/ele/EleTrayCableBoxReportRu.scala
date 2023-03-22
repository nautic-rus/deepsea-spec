package local.pdf.ru.ele

import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf.{PdfDocument, PdfReader, PdfWriter, WriterProperties}
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import deepsea.elec.{ElecHelper, ElecManager}
import deepsea.elec.ElecManager.{CableBoxesBySystem, TrayAndCableBox, TraysBySystem}
import deepsea.materials.MaterialsHelper
import deepsea.pipe.PipeManager.{Material, Units}
import local.common.DBRequests.findChess
import local.domain.CommonTypes.DrawingChess
import local.pdf.UtilsPDF
import local.pdf.en.accom.AccomReportEn.{getUnits, mmToPt}
import local.pdf.en.common.ReportCommonEN.{DocNameEN, Item11ColumnsEN, border5mm, defaultFontSize, fillStamp, fontHELVETICA, getNnauticLigoEN, stampEN}
import org.davidmoten.text.utils.WordWrap

import scala.jdk.CollectionConverters.CollectionHasAsScala
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, OutputStream}
import java.nio.file.Files
import scala.collection.mutable.ListBuffer

object EleTrayCableBoxReportRu extends UtilsPDF with ElecHelper{
  private val pageSize: PageSize = PageSize.A4

  private val pointColumnWidths = Array(
    mmToPt(15),
    mmToPt(90),
    mmToPt(34),
    mmToPt(10),
    mmToPt(15),
    mmToPt(20),
    mmToPt(11),
    mmToPt(11) + 0 //207
  )
  val units: List[Units] = getUnits

  def genTraysAndCBListEnPDF(project: String, docNumber: String, docName: String, rev: String, lang: String): String = {

   /* val materials: List[Material] = getMaterials*/
    val rawData: TrayAndCableBox = TrayAndCableBox(getTraysBySystem(project, docNumber), getCableBoxesBySystem(project, docNumber))
    val filePath: String = Files.createTempDirectory("traysCbPdf").toAbsolutePath.toString + File.separator + docNumber + "_rev" + rev + ".pdf"
    val rows: List[Item11ColumnsEN] = genRows(rawData, docNumber, rev, lang)
    val totalRows: List[Item11ColumnsEN] = genTotalRows(rawData, docNumber, rev, lang)
    val dn = DocNameEN(num = docNumber, name = docName, lastRev = if (rev != "") rev else "0")
    processPDF(dn, filePath, rows, totalRows)
    filePath
  }

  private def processPDF(docNameEN: DocNameEN, path: String, items: List[Item11ColumnsEN], totalItems: List[Item11ColumnsEN], genAll: Boolean = true): Unit = {

    val pdfWriter: PdfWriter = new PdfWriter(path, new WriterProperties().setFullCompressionMode(true)) {
      setSmartMode(true)
    }
    /*       val pdfWriter: PdfWriter = new PdfWriter("c:\\35\\200101-304-0001_rev0.pdf", new WriterProperties().setFullCompressionMode(true)) {
              setSmartMode(true)
            }*/
    val pdfDoc = new PdfDocument(pdfWriter) {
      initializeOutlines()
    }
    val doc: Document = new Document(pdfDoc, pageSize)

    val titul: PdfDocument = genTitulA4(docNameEN)
    titul.copyPagesTo(1, 1, pdfDoc)

    generatePartListPages(docNameEN, totalItems, "BILLING OF MATERIALS").foreach(page => {
      page.copyPagesTo(1, 1, pdfDoc)
    })

    if (genAll) {
      generatePartListPages(docNameEN, items, "PART LIST").foreach(page => {
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
    val logo = getNnauticLigoEN.scaleToFit(72, 20).setFixedPosition(mmToPt(9), mmToPt(8))
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

  private def generatePartListPages(docNameEN: DocNameEN, items: List[Item11ColumnsEN], header: String): List[PdfDocument] = {
    val pages: ListBuffer[PartListBodyPage] = ListBuffer.empty[PartListBodyPage]
    pages += new PartListBodyPage(docNameEN)

    pages.last.insertRow(Item11ColumnsEN(true, A1 = header))

    var currPage = 1
    items.foreach(row => {
      val wrappedRows: List[Item11ColumnsEN] = generateWrappedRows(row)
      if (!pages.last.hasRoom(wrappedRows.length + 2)) {
        pages += new PartListBodyPage(docNameEN)
        currPage = currPage + 1
      }
      pages.last.insertRows(wrappedRows)
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

  private def genRows(rawData: TrayAndCableBox, docNumber: String, rev: String, lang: String): List[Item11ColumnsEN] = {
    val chess: DrawingChess = {
      val l = findChess(docNumber, rev)
      if (l.nonEmpty) l.head else DrawingChess()
    }

    val trays: List[TraysBySystem] =rawData.trays
    val cbs: List[CableBoxesBySystem] =rawData.cableBoxes

    val traysGrouped: List[Item11ColumnsEN] ={
      val ret = ListBuffer.empty[Item11ColumnsEN]
      trays.groupBy(s=>s.stockCode).foreach(gr=>{
        val item=gr._2.head
        val wght: Double =gr._2.map(_.weight).sum
        val len=gr._2.map(_.length).sum
        val mat = item.material.name(lang)
        val matDescr = item.material.description(lang)
        val unit = "метр"
        val weight: String = if (wght < 0.01) " 0.01" else String.format("%.2f", wght)
        val qty =String.format("%.2f",len)
        ret += Item11ColumnsEN(A1 = "", A2 = mat, A3 = unit, A4 = qty, A5 = weight, A6 = matDescr, A7 = "", A8 = "", A12 = item.material.code)
      })
      ret.toList
    }
    val cbsGrouped: List[Item11ColumnsEN] = {
      val ret = ListBuffer.empty[Item11ColumnsEN]
      cbs.groupBy(s => s.stockCode).foreach(gr => {
        val item = gr._2.head
        val wght: Double = gr._2.map(_.weight).sum
        val mat = item.material.name(lang)
        val matDescr = item.material.description(lang)
        val unit = "шт"
        val weight: String = if (wght < 0.01) " 0.01" else String.format("%.2f", wght)
        val qty = gr._2.length.toString
        ret += Item11ColumnsEN(A1 = "", A2 = mat, A3 = unit, A4 = qty, A5 = weight, A6 = matDescr, A7 = "", A8 = "", A12 = item.material.code)
      })
      ret.toList
    }

    val totalItems: List[Item11ColumnsEN] = traysGrouped ++ cbsGrouped

    val cbsRows: List[Item11ColumnsEN] = {
      val ret = ListBuffer.empty[Item11ColumnsEN]
      cbs.foreach(cb => {
        val id=cb.userId
        val item = cb
        val mat = item.material.name(lang)
        val matDescr = item.material.description(lang)
        val unit = "шт"
        val weight: String = if (cb.weight < 0.01) " 0.01" else String.format("%.2f", cb.weight)
        val qty = 1.toString
        ret += Item11ColumnsEN(A1 = id, A2 = mat, A3 = unit, A4 = qty, A5 = weight, A6 = matDescr, A7 = "", A8 = "", A12 = item.material.code)
      })
      ret.toList
    }

    cbsRows.sortBy(s => s.A1).toList.sortBy(s => if (s.A1.contains(".")) s.A1.split("\\.").map(s => s.reverse.padTo(10 - s.length, '0').reverse).mkString("") else s.A1.reverse.padTo(10 - s.A1.length, '0').reverse)
  }

  private def genTotalRows(rawData: TrayAndCableBox, docNumber: String, rev: String, lang: String): List[Item11ColumnsEN] = {
    val chess: DrawingChess = {
      val l = findChess(docNumber, rev)
      if (l.nonEmpty) l.head else DrawingChess()
    }
    val trays: List[TraysBySystem] = rawData.trays
    val cbs: List[CableBoxesBySystem] = rawData.cableBoxes
    val traysGrouped: List[Item11ColumnsEN] = {
      val ret = ListBuffer.empty[Item11ColumnsEN]
      trays.groupBy(s => s.stockCode).foreach(gr => {
        val item = gr._2.head
        val wght: Double = gr._2.map(_.weight).sum
        val len = gr._2.map(_.length).sum
        val mat = item.material.name(lang)
        val matDescr = item.material.description(lang)
        val unit = "метр"
        val weight: String = if (wght < 0.01) " 0.01" else String.format("%.2f", wght)
        val qty = String.format("%.2f", len)
        ret += Item11ColumnsEN(A1 = "", A2 = mat, A3 = unit, A4 = qty, A5 = weight, A6 = matDescr, A7 = "", A8 = "", A12 = item.material.code)
      })
      ret.toList
    }
    val cbsGrouped: List[Item11ColumnsEN] = {
      val ret = ListBuffer.empty[Item11ColumnsEN]
      cbs.groupBy(s => s.stockCode).foreach(gr => {
        val item = gr._2.head
        val wght: Double = gr._2.map(_.weight).sum
        val mat = item.material.name(lang)
        val matDescr = item.material.description(lang)
        val unit = "шт"
        val weight: String = if (wght < 0.01) " 0.01" else String.format("%.2f", wght)
        val qty = gr._2.length.toString
        ret += Item11ColumnsEN(A1 = "", A2 = mat, A3 = unit, A4 = qty, A5 = weight, A6 = matDescr, A7 = "", A8 = "", A12 = item.material.code)
      })
      ret.toList
    }
    val totalItems: List[Item11ColumnsEN] = traysGrouped ++ cbsGrouped
    totalItems
  }

  private def formatUnits(mat: Material): String = {
    units.find(p => p.code.equals(mat.units)) match {
      case Some(value) => value.thumb
      case None => "NA"
    }
  }

  private def formatUnitsStr(unit: String, lang: String): String = {
    units.find(p => p.code.equals(unit)) match {
      case Some(value) => value.thumb(lang)
      case None => "NA"
    }
  }

  private def formatWGTDouble(ps: Double): String = if (ps < 0.01) " 0.01" else String.format("%.2f", ps)

  private def generateWrappedRows(item: Item11ColumnsEN): List[Item11ColumnsEN] = {
    val A1count = 12
    val A2count = 60
    val A3count = 25
    val A4count = 20
    val A5count = 16
    val A6count = 20
    val A7count = 7
    val A8count = 10
    val A9count = 10
    val A10count = 26
    val A11count = 26
    val buff = ListBuffer.empty[Item11ColumnsEN]
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
        buff += new Item11ColumnsEN(
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
          A11.lift(i).getOrElse(""),
        )
      })
    }
    buff.toList
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
      }

      generateRows()

      cellBuff.foreach(cell => {
        table.addCell(cell)
      })

      processStaticText()

      def processStaticText(): Unit = {
        setStampText(cellBuff(0), "N", italic = false, bold = true)
        setStampText(cellBuff(1), "MATERIAL", italic = false, bold = true)
        setStampText(cellBuff(2), "DOC", italic = false, bold = true)
        setStampText(cellBuff(3), "UNIT", italic = false, bold = true)
        setStampText(cellBuff(4), "QTY", italic = false, bold = true)
        setStampText(cellBuff(5), "WGT, kg", italic = false, bold = true)
        setStampText(cellBuff(6), "ROOM", italic = false, bold = true)
        setStampText(cellBuff(7), "POS", italic = false, bold = true)
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
        bodyGrid.addCell(generateCellLeftAlign(item.A6))
        bodyGrid.addCell(generateCell(item.A3))
        bodyGrid.addCell(generateCell(item.A4))
        bodyGrid.addCell(generateCell(item.A5))
        bodyGrid.addCell(generateCell(item.A7))
        bodyGrid.addCell(generateCell(item.A8))
      }
      else {
        bodyGrid.addCell(generateSpannedCellBold(item.A1))
      }
      currentRow = currentRow + 1
    }

    def insertRows(items: List[Item11ColumnsEN]): Unit = {
      items.foreach(item => {
        if (!item.isHeader) {
          bodyGrid.addCell(generateCellDiffSize(item.A1, 3))
          bodyGrid.addCell(generateCellLeftAlign(item.A2))
          bodyGrid.addCell(generateCellLeftAlign(item.A6))
          bodyGrid.addCell(generateCell(item.A3))
          bodyGrid.addCell(generateCell(item.A4))
          bodyGrid.addCell(generateCell(item.A5))
          bodyGrid.addCell(generateCell(item.A7))
          bodyGrid.addCell(generateCell(item.A8))
        }
        else {
          bodyGrid.addCell(generateSpannedCellBold(item.A1))
        }
        currentRow = currentRow + 1
      })
    }
  }

}
