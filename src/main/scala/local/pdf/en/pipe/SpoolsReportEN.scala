package local.pdf.en.pipe

import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf.{PdfDocument, PdfReader, PdfWriter, WriterProperties}
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Div, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import deepsea.pipe.PipeHelper
import deepsea.pipe.PipeManager.{Material, PipeSeg}
import local.pdf.UtilsPDF
import local.pdf.en.common.ReportCommonEN.{DocNameEN, Item11ColumnsEN, border5mm, defaultFontSize, fillStamp, fontHELVETICA, getNnauticLigoEN, rowwrap, stampEN}
import org.davidmoten.text.utils.WordWrap

import scala.jdk.CollectionConverters.CollectionHasAsScala
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, OutputStream}
import java.nio.file.Files
import java.util
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


  def genSpoolsListEnPDF(docNumber: String, docName: String, rev: String, rawData: List[PipeSeg], lang: String = "ru", genAll: Boolean = false, materials: List[Material]): String = {
    val filePath: String = Files.createTempDirectory("spoolPdf").toAbsolutePath.toString + File.separator + docNumber + "ML_rev" + rev + ".pdf"
    val rows: List[Item11ColumnsEN] = genRows(rawData, lang)
    val totalRows: List[Item11ColumnsEN] = genTotal(rows, lang, materials)
    val dn = DocNameEN(num = docNumber, name = docName, lastRev = if (rev != "") rev else "0")
    processPDF(dn, filePath, rows, totalRows, genAll, lang)

    filePath
  }

  def genSpoolsListEnPDFAll(docNumber: String, docName: String, rev: String, rawData: List[PipeSeg], lang: String = "ru", materials: List[Material]): String = {
    val filePath: String = Files.createTempDirectory("spoolPdf").toAbsolutePath.toString + File.separator + docNumber + "ML_rev" + rev + ".pdf"
    val rows: List[Item11ColumnsEN] = genRows(rawData, lang)
    val totalRows: List[Item11ColumnsEN] = genTotal(rows, lang, materials)
    val dn = DocNameEN(num = docNumber, name = docName, lastRev = if (rev != "") rev else "0")
    processPDF(dn, filePath, rows, totalRows, genAll = true, lang)
    filePath
  }

  private def processPDF(docNameEN: DocNameEN, path: String, itemsIN: List[Item11ColumnsEN], totalItemsIN: List[Item11ColumnsEN], genAll: Boolean = false, lang: String = "en"): Unit = {
    val items: List[Item11ColumnsEN] = {
      val buff = ListBuffer.empty[Item11ColumnsEN]
      if (lang.equals("ru")) {
        itemsIN.foreach(row => {
          row.A3 match {
            case "pcs" => buff += row.copy(A3 = "шт")
            case "m" => buff += row.copy(A3 = "м")
            case _ => buff += row
          }
        })
        buff.toList
      } else {
        itemsIN
      }
    }
    val totalItems: List[Item11ColumnsEN] = {
      val buff = ListBuffer.empty[Item11ColumnsEN]
      if (lang.equals("ru")) {
        totalItemsIN.foreach(row => {
          row.A3 match {
            case "pcs" => buff += row.copy(A3 = "шт")
            case "m" => buff += row.copy(A3 = "м")
            case _ => buff += row
          }
        })
        buff.toList
      } else {
        totalItemsIN
      }
    }

    val pdfWriter: PdfWriter = new PdfWriter(path, new WriterProperties().setFullCompressionMode(true)) {
      setSmartMode(true)
    }
    val pdfDoc = new PdfDocument(pdfWriter) {
      initializeOutlines()
    }
    val doc: Document = new Document(pdfDoc, pageSize)

    val titul: PdfDocument = genTitulA4(docNameEN)
    titul.copyPagesTo(1, 1, pdfDoc)

    generatePartListPages(docNameEN, totalItems, lang).foreach(page => {
      page.copyPagesTo(1, 1, pdfDoc)
    })

    if (genAll) {
      generatePartListPages(docNameEN, items, lang).foreach(page => {
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

  private def generatePartListPages(docNameEN: DocNameEN, items: List[Item11ColumnsEN], lang: String = "en"): List[PdfDocument] = {
    val pages: ListBuffer[PartListBodyPage] = ListBuffer.empty[PartListBodyPage]
    pages += new PartListBodyPage(docNameEN, lang)
    var currPage = 1
    var lastA1 = ""
    items.foreach(row => {

      val wrappedRows: List[Item11ColumnsEN] = {
        val isNeedDummy = (lastA1.nonEmpty && row.A1.nonEmpty && !row.A1.startsWith(lastA1))
        val ret = generateWrappedRows(row)
        if (isNeedDummy) {
          val ret2 = ret.reverse.toBuffer
          ret2 += Item11ColumnsEN(isHeader = false, A1 = "")
          ret2.reverse.toList
        } else {
          ret
        }
      }

      if (!pages.last.hasRoom(wrappedRows.length + 2)) {
        pages += new PartListBodyPage(docNameEN, lang)
        currPage = currPage + 1
      }
      pages.last.insertRows(wrappedRows)

      lastA1 = row.A1.split('.').headOption match {
        case Some(v) => v
        case None => lastA1
      }
    })
    /*
        items.foreach(row => {
          if (!pages.last.hasRoom(2)) {
            pages += new PartListBodyPage(docNameEN, lang)
            currPage = currPage + 1
          }
          pages.last.insertRow(row)
        })

        */

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


  private class PartListBodyPage(docNameEN: DocNameEN, lang: String = "en") {

    private val N = {
      lang match {
        case "en" => "N"
        case "ru" => "№"
        case _ => "N"
      }
    }

    private val MATERIAL = {
      lang match {
        case "en" => "MATERIAL"
        case "ru" => "МАТЕРИАЛ"
        case _ => "MATERIAL"
      }
    }
    private val UNIT = {
      lang match {
        case "en" => "UNIT"
        case "ru" => "ЕД.ИЗМ."
        case _ => "UNIT"
      }
    }

    private val QTY = {
      lang match {
        case "en" => "QTY"
        case "ru" => "КОЛ-ВО"
        case _ => "QTY"
      }
    }

    private val WGT = {
      lang match {
        case "en" => "WGT, kg"
        case "ru" => "ВЕС, кг"
        case _ => "WGT, kg"
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
      }

      generateRows()

      cellBuff.foreach(cell => {
        table.addCell(cell)
      })

      processStaticText()

      def processStaticText(): Unit = {
        setStampText(cellBuff(0), N, italic = false, bold = true)
        setStampText(cellBuff(1), MATERIAL, italic = false, bold = true)
        setStampText(cellBuff(2), UNIT, italic = false, bold = true)
        setStampText(cellBuff(3), QTY, italic = false, bold = true)
        setStampText(cellBuff(4), WGT, italic = false, bold = true)
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

    def insertRows(items: List[Item11ColumnsEN]): Unit = {
      items.foreach(item => {
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
      })
    }

  }

  private def genRows(rawData: List[PipeSeg], lang: String): List[Item11ColumnsEN] = {
    val rows: ListBuffer[Item11ColumnsEN] = ListBuffer.empty[Item11ColumnsEN]

    val rowsQTY: ListBuffer[PipeSeg] = ListBuffer.empty[PipeSeg]
    rawData.groupBy(s => (s.spool, s.material.code, s.material.name)).foreach(gr => {
      val master: PipeSeg = gr._2.head
      val qty: Double = master.material.units match {
        case "796" => master.typeCode match {
          case "JOINT" => gr._2.map(_.length).sum
          case "AUX" => gr._2.map(_.length).sum
          case _ => gr._2.length
        }
        case _ => gr._2.map(_.length).sum
      }
      val wgt: Double = gr._2.map(_.weight).sum
      rowsQTY += (master.copy(length = qty, weight = wgt))
    })

    val spoolsCount = ListBuffer.empty[String]
    rowsQTY.sortBy(x => x.spool + "-" + x.material.name).foreach(row => {
      val id = if (row.compType != "AUX"){
        spoolsCount += row.spool
        val spoolId = formatSpoolId(row.spool, spoolsCount.count(_ == row.spool).toString)
        if (spoolId.contains(".")) spoolId.split('.').head else spoolId
      }
      else{
        row.spool
      }
      //val id = formatSpoolId(row.spool, row.spPieceId.toString)
      //val mat = row.material.code+ " "+ row.material.name
      val matName = row.material.name(lang)
      val matDesc = row.material.description(lang)
      val mat = if (matDesc != "") matName + " (" + matDesc + ")" else matName
      val qty = formatQTY(row)
      val unit = formatUnits(row.material, lang)
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
          case "FWNR" => "1"
          case "FWRE" => "1"
          case "FLAN" => "1"
          case "HVAC" => ps.length.toString
          case _ =>
            if (ps.length < 1.0)
              "1"
            else
              Math.ceil(ps.length).toInt.toString
        }
      }
      case "166" => String.format("%.2f", ps.weight)

      case _ => String.format("%.1f", ps.length)

    }
  }

  private def formatWGT(ps: PipeSeg): String = {
    ps.typeCode match {
      case "PIPE" => String.format("%.2f", ps.weight)
      case "HVAC" =>
        ps.typeDesc match {
          case "PLATE" => String.format("%.2f", ps.weight)
          case _ =>
            ps.compType match {
              case "A" => String.format("%.2f", ps.material.singleWeight)
              case "B" => String.format("%.2f", ps.material.singleWeight)
              case _ => String.format("%.2f", ps.weight)
            }
        }
      case _ => {
        val qty = if (ps.length == 0.0) 1 else ps.length
        val w = ps.material.singleWeight * qty
        if (w < 0.01) " 0.01" else String.format("%.2f", w)
      }
    }
  }

  private def formatUnits(mat: Material, lang: String): String = {
    mat.units match {
      case "006" => if (lang == "ru") "м" else "m"
      case "796" => if (lang == "ru") "шт" else "pcs"
      case "166" => if (lang == "ru") "кг" else "kg"
      case _ => "NA"
    }
  }

  private def genTotal(in: List[Item11ColumnsEN], lang: String, materials: List[Material]): List[Item11ColumnsEN] = {
    val buff = ListBuffer.empty[Item11ColumnsEN]
    in.groupBy(s => s.A12).foreach(gr => {
      val ent = gr._2.head
      val qty = {
        val w = gr._2.map(_.A4.toDoubleOption.getOrElse(0.0)).sum
        if (ent.A3.equals("pcs") || ent.A3 == "шт") {
          Math.ceil(w).toInt.toString
        } else {
          if (w < 0.01) " 0.01" else String.format("%.2f", w)
        }
      }
      val wgt = {
        val w = gr._2.map(_.A5.toDoubleOption.getOrElse(0.0)).sum
        if (w < 0.01) " 0.01" else String.format("%.2f", w)
      }
      val materialName = materials.find(_.code == ent.A12) match {
        case Some(value) => value.name(lang)
        case _ => ent.A2
      }
      buff += Item11ColumnsEN(A1 = "", A2 = materialName, A3 = ent.A3, A4 = qty, A5 = wgt)
    })
    buff.sortBy(s => s.A2).toList
  }

  private def generateWrappedRows(item: Item11ColumnsEN): List[Item11ColumnsEN] = {
    val A1count = 12
    val A2count = 75
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
      val A1 = rowwrap(item.A1, A1count)
      val A2 = rowwrap(item.A2, A2count)
      val A3 = rowwrap(item.A3, A3count)
      val A4 = rowwrap(item.A4, A4count)
      val A5 = rowwrap(item.A5, A5count)
      val A6 = rowwrap(item.A6, A6count)
      val A7 = rowwrap(item.A7, A7count)
      val A8 = rowwrap(item.A8, A8count)
      val A9 = rowwrap(item.A9, A9count)
      val A10 = rowwrap(item.A10, A10count)
      val A11 = rowwrap(item.A11, A11count)

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


}
