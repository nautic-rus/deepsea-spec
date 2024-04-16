package deepsea.materials

import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf.action.PdfAction
import com.itextpdf.kernel.pdf.navigation.PdfExplicitDestination
import com.itextpdf.kernel.pdf._
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import deepsea.esp.EspManager.{GlobalEsp, GlobalEspSpec}
import deepsea.pipe.PipeManager.{MaterialStatement, SpecMaterial}
import local.common.DBRequests.{MaterialNode, findChess}
import local.pdf.UtilsPDF
import local.pdf.en.common.ReportCommonEN._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, OutputStream}
import java.nio.file.Files
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.CollectionHasAsScala

trait MaterialPdf extends UtilsPDF with MaterialsHelper {

  private val pageSize: PageSize = PageSize.A3.rotate()
  private val pointColumnWidths = Array(
    mmToPt(15),
    mmToPt(35),
    mmToPt(117.5),
    mmToPt(70),
    mmToPt(50),
    mmToPt(50),
    mmToPt(20),
    mmToPt(20),
    mmToPt(20),
    mmToPt(20),
  )
  val defaultFontSize = mmToPt(3.0)


  def createMaterialsSummaryPdf(projectId: Int, statemId: Int, materials: List[GlobalEspSpec]): String = {
    val stmts = getMaterialStatements.filter(_.project_id == projectId)
    val rows = fillRows(materials, statemId, stmts, first = true)
    val dn: DocNameEN = stmts.find(_.id == statemId) match {
      case Some(statement) =>  DocNameEN(num = statement.doc_number, name = statement.name)
      case _ =>  DocNameEN(num = "NO DOC NUMBER", name = "NO DOC NAME")
    }
    val path: String = stmts.find(_.id == statemId) match {
      case Some(statement) =>
        Files.createTempDirectory("statement").toAbsolutePath.toString + "/" + statement.doc_number + ".pdf"
      case _ =>
        Files.createTempDirectory("statement").toAbsolutePath.toString + "/" + "undefined" + ".pdf"
    }
    processPDF(dn, path, rows)
    path
  }
  def fillRows(materials: List[GlobalEspSpec], parent_id: Int, statements: List[MaterialStatement], first: Boolean = false): List[Item11ColumnsEN] = {
    val rows = ListBuffer.empty[Item11ColumnsEN]
    statements.filter(_.id == parent_id).foreach(stmt => {
      val stMaterials = materials.filter(_.material.statem_id == stmt.id)
      if (stMaterials.nonEmpty){
        if (!first){
          rows += Item11ColumnsEN(isHeader = true, A1 = stmt.code + " - " + stmt.name)
        }
        stMaterials.foreach(stMaterial => {
          rows += Item11ColumnsEN(A1 = alz((stMaterials.indexOf(stMaterial) + 1).toString, 4),
            A2 = stMaterial.code,
            A3 = stMaterial.material.name,
            A4 = stMaterial.material.descr,
            A5 = stMaterial.material.supplier,
            A6 = stMaterial.material.note,
            A7 = stMaterial.units,
            A8 = (Math.round(stMaterial.qty * 100) / 100d).toString,
            A9 = (Math.round(stMaterial.material.weight * 100) / 100d).toString,
            A10 = (Math.round(stMaterial.weightTotal * 100) / 100d).toString)
        })
      }
    })
    statements.filter(_.parent_id == parent_id).foreach(stmt => {
      rows ++= fillRows(materials, stmt.id, statements)
    })
    rows.toList
  }
  def orderDot(input: String): String = {
    input.split("\\.").map(alz(_)).mkString
  }
  def alz(input: String, length: Int = 10) = {
    ("0" * length + input).takeRight(length)
  }
  private def processPDF(docNameEN: DocNameEN, file: String, items: List[Item11ColumnsEN]): Unit = {

    val pdfWriter: PdfWriter = new PdfWriter(file, new WriterProperties().setFullCompressionMode(true)) {
      setSmartMode(true)
    }
    val pdfDoc = new PdfDocument(pdfWriter) {
      initializeOutlines()
    }
    val doc: Document = new Document(pdfDoc, pageSize)

    val titul: PdfDocument = genTitulA4(docNameEN)

    titul.copyPagesTo(1, 1, pdfDoc)

    generatePartListPages(docNameEN, items).foreach(page => {
      page.copyPagesTo(1, 1, pdfDoc)
    })
    (1 to pdfDoc.getNumberOfPages).foreach(i => {
      if (i == 1) {
        val pa = new Paragraph("1/" + pdfDoc.getNumberOfPages.toString)
        pa.setFontSize(mmToPt(5))
        pa.setFont(fontHELVETICA)
        doc.showTextAligned(pa, mmToPt(203f + 210), mmToPt(12), i, TextAlignment.RIGHT, VerticalAlignment.TOP, 0)
      } else {
        val pa = new Paragraph(i.toString)
        pa.setFontSize(mmToPt(5))
        pa.setFont(fontHELVETICA)
        doc.showTextAligned(pa, mmToPt(201.5f + 210), mmToPt(12), i, TextAlignment.RIGHT, VerticalAlignment.TOP, 0)
      }
    })

    //GENERATE OUTLINES
//    val root: PdfOutline = pdfDoc.getOutlines(false)
//    var subRoot: PdfOutline = null
//    descrTreeBuffer.foreach(descr => {
//      val page = pdfDoc.getPage(descr.pageNum + 1)
//      val dest: PdfExplicitDestination = PdfExplicitDestination.createFitH(page, descr.offset)
//      val action = PdfAction.createGoTo(dest)
//      if (descr.isRoot) {
//        subRoot = root.addOutline(descr.A1 + " " + descr.A2)
//        subRoot.addAction(action)
//      } else {
//        val sublocal = subRoot.addOutline(descr.A1 + " " + descr.A2)
//        sublocal.addAction(action)
//      }
//    })
    doc.close()
  }

  private def generatePartListPages(docNameEN: DocNameEN, items: List[Item11ColumnsEN]): List[PdfDocument] = {
    val pages: ListBuffer[PartListBodyPage] = ListBuffer.empty[PartListBodyPage]
    pages += new PartListBodyPage(docNameEN, 0)
    var currPage = 1
    items.foreach(row => {
      if (!pages.last.hasRoom(2)) {
        pages += new PartListBodyPage(docNameEN, currPage)
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

  private class DS(val rootNode: MaterialNode, val in: List[GlobalEsp], private val materialNodes: List[MaterialNode]) {
    val project: String = rootNode.project
    val currenLevel: Int = rootNode.data.length

    def this(ds: DS) {
      this(ds.rootNode, ds.in, ds.materialNodes)
    }

    def isEmpty: Boolean = in.isEmpty

    def nextLevel(): List[DS] = {
      currenLevel match {
        case 0 => Level(3)
        case 3 => Level(6)
        case 6 => Level(9)
        case 9 => Level(12)
        case 12 => lastLevel()
        case _ => {
          List.empty[DS]
        }
      }
    }

    private def lastLevel(): List[DS] = {
      val buff = ListBuffer.empty[DS]

      buff += new DS(MaterialNode(project = project, label = "", label_ru = "", data = "", user = "", date = 0), in, materialNodes)

      buff.toList
    }

    private def Level(f: Int): List[DS] = {
      val buff = ListBuffer.empty[DS]
      in.groupBy(s => s.code.take(f)).foreach(gr => {
        materialNodes.find(d => d.data.equals(gr._1)) match {
          case Some(value: MaterialNode) => buff += new DS(value, gr._2, materialNodes)
          case None => buff += new DS(defaultNoNode(gr._1), gr._2, materialNodes)
        }
      })
      buff.toList
    }

    private def defaultNoNode(in: String): MaterialNode = {
      MaterialNode(project, "Not Set", "БЕЗ КАТЕГОРИИ", in, "NO", 9999999)
    }

    override def toString = s"parent: ${currenLevel} (${rootNode.data})${rootNode.label_ru} values count: ${in.length}"
  }

  private class PartListBodyPage(docNameEN: DocNameEN, currPage: Int) {

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
    val logo = getNnauticLigoEN.scaleToFit(72, 20).setFixedPosition(mmToPt(9 + 210), mmToPt(8))
    doc.add(logo)

    val stamp: Table = stampENLandscape()
    fillStampLandscape(doc, docNameEN)

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
        setStampText(cellBuff(0), "№", italic = false, bold = true)
        setStampText(cellBuff(1), "MAT.CODE / КОД МАТ.", italic = false, bold = true)
        setStampText(cellBuff(2), "TITLE / НАИМЕНОВАНИЕ", italic = false, bold = true)
        setStampText(cellBuff(3), "DESCR. / ОБОЗН.", italic = false, bold = true)
        setStampText(cellBuff(4), "SUPPLIER / ПОСТАВЩИК", italic = false, bold = true)
        setStampText(cellBuff(5), "NOTE / ПРИМ.", italic = false, bold = true)
        setStampText(cellBuff(6), "UNITS / ЕД.", italic = false, bold = true)
        setStampText(cellBuff(7), "QTY / К-ВО", italic = false, bold = true)
        setStampText(cellBuff(8), "WGT / ВЕС", italic = false, bold = true)
        setStampText(cellBuff(9), "T.WGT / О.ВЕС", italic = false, bold = true)
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

    def generateCell(in: String, ha: TextAlignment = TextAlignment.CENTER, pading: Int = 0): Cell = {
      new Cell(2, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
        .setHorizontalAlignment(HorizontalAlignment.CENTER)
        .add(new Paragraph(in)
          .setTextAlignment(ha)
          .setFontSize(defaultFontSize)
          .setFont(fontHELVETICA)
          .setPaddingLeft(pading)
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
        bodyGrid.addCell(generateCell(item.A1))
        bodyGrid.addCell(generateCell(item.A2, TextAlignment.LEFT, 5))
        bodyGrid.addCell(generateCell(item.A3, TextAlignment.LEFT, 5))
        bodyGrid.addCell(generateCell(item.A4, TextAlignment.LEFT, 5))
        bodyGrid.addCell(generateCell(item.A5))
        bodyGrid.addCell(generateCell(item.A6))
        bodyGrid.addCell(generateCell(item.A7))
        bodyGrid.addCell(generateCell(item.A8))
        bodyGrid.addCell(generateCell(item.A9))
        bodyGrid.addCell(generateCell(item.A10))
      }
      else {
        bodyGrid.addCell(generateSpannedCellBold(item.A1))
      }
      currentRow = currentRow + 1
    }

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
    val logo = getNnauticLigoEN.scaleToFit(72, 20).setFixedPosition(mmToPt(9 + 210), mmToPt(8))
    doc.add(logo)
    border5mm(pdfDoc.getPage(1))
    val stamp: Table = stampENLandscape()
    fillStampLandscape(doc, docNameEN)
    doc.add(stamp)
    doc.close()
    os.flush()
    os.close()
    new PdfDocument(new PdfReader(new ByteArrayInputStream(os.asInstanceOf[ByteArrayOutputStream].toByteArray)))
  }
}
