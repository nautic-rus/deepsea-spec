package local.pdf.ru.order

import scala.jdk.CollectionConverters.CollectionHasAsScala
import com.itextpdf.kernel.geom.PageSize
import com.itextpdf.kernel.pdf.action.PdfAction
import com.itextpdf.kernel.pdf.navigation.PdfExplicitDestination
import com.itextpdf.kernel.pdf.{PdfDocument, PdfOutline, PdfReader, PdfWriter, WriterProperties}
import com.itextpdf.layout.Document
import com.itextpdf.layout.element.{Cell, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import deepsea.database.DBManager.GetMongoConnection
import deepsea.esp.EspManager
import deepsea.esp.EspManager.GlobalEsp
import deepsea.materials.MaterialsHelper
import local.common.DBRequests.MaterialNode
import local.pdf.UtilsPDF
import local.pdf.en.common.ReportCommonEN.{DocNameEN, Item11ColumnsEN, border5mm, defaultFontSize, fillStamp, fillStampLandscape, fontHELVETICA, getNnauticLigoEN, stampEN, stampENLandscape}
import local.pdf.en.prd.PrdPartsReportEN.{mmToPt, pageSize, pointColumnWidths}
import local.pdf.ru.ele.EleTrayCableBoxReportRu.{generateDeviceGlobalEsp, generateHullGlobalEsp, generatePipeGlobalEsp}
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.CodecRegistry
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.model.Filters.equal
import org.mongodb.scala.{MongoCollection, MongoDatabase}

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File, FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream, OutputStream}
import java.nio.file.Files
import java.util.Date
import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

object OrderReportV1 extends UtilsPDF with MaterialsHelper {
  private case class DescrTree(pageNum: Int, A1: String, A2: String, isRoot: Boolean = false, offset: Float = 0.0f)

  private val rootNames: List[MaterialNode] = {
    val buf = ListBuffer.empty[MaterialNode]
    buf += MaterialNode("200101", "200101-101-102", "Ведомость заказа корпусного металла (с сертификатом РС)", "HUL", "", 0)
    buf += MaterialNode("200101", "200101-101-103", "Ведомость заказа трубного проката и фасонных частей", "SYS", "", 0)
    buf += MaterialNode("200101", "200101-101-104", "Ведомость заказа арматуры", "VAL", "", 0)
    buf += MaterialNode("200101", "200101-101-105", "Ведомость ДБА", "XX1", "", 0)
    buf += MaterialNode("200101", "200101-101-106", "Ведомость АСИ и ППИ", "XX2", "", 0)
    buf += MaterialNode("200101", "200101-101-107", "Ведомость окраски и грунтовки корпуса", "XX3", "", 0)
    buf += MaterialNode("200101", "200101-101-108", "Ведомость изоляции и зашивок", "ROM", "", 0)
    buf += MaterialNode("200101", "200101-101-111", "Ведомость заказа ЭСН (Электро-слесарное насыщение)", "ESN", "", 0)
    buf += MaterialNode("200101", "200101-101-112", "Ведомость заказа КИП", "XX4", "", 0)
    buf += MaterialNode("200101", "200101-101-113", "Ведомость ЗИП", "XX5", "", 0)
    buf += MaterialNode("200101", "200101-101-114", "Ведомость заказа изделий МСЧ", "MCH", "", 0)
    buf += MaterialNode("200101", "200101-101-115", "Ведомость заказа общих материалов", "COM", "", 0)
    buf += MaterialNode("200101", "200101-101-101", "Ведомость заказа оборудования", "EQU", "", 0)
    buf.toList
  }

  private val descrTreeBuffer = ListBuffer.empty[DescrTree]
  private val pageSize: PageSize = PageSize.A3.rotate()
  private val pointColumnWidths = Array(
    mmToPt(10),
    mmToPt(160),
    mmToPt(87),
    mmToPt(10),

    mmToPt(25),
    mmToPt(25),
    mmToPt(25),
    mmToPt(25),

    mmToPt(41),

    mmToPt(5),

    mmToPt(5) + 0 //
  )


  def generateOrderPDF(project: String, code: String, user: String = "NoName"): String = {
    val dbProject = "n002"
    val file = File.createTempFile("orderList", ".pdf")
    descrTreeBuffer.clear()
    if (code.length >= 3) {
      val materialNodes = getMaterialNodes(project)

      materialNodes.find(s => s.data.equals(code)) match {
        case Some(rn) => {
          rootNames.find(s => s.data.equals(rn.data.take(3))) match {
            case Some(doc) => {
              val dn: DocNameEN = DocNameEN(num = doc.label, name = doc.label_ru, user = user)

              val in: List[GlobalEsp] = {
                val hull = generateHullGlobalEsp(List(dbProject))
                val pipe = generatePipeGlobalEsp(List(dbProject))
                val device = generateDeviceGlobalEsp(List(dbProject))

                val prebuff = hull ++ pipe ++ device
                val buff=ListBuffer.empty[GlobalEsp]
                prebuff.groupBy(s => s.code).foreach(gr => {
                  val qty = gr._2.map(_.qty).sum
                  val w = gr._2.map(_.weight).sum
                  val docss = gr._2.flatMap(_.documents)
                  buff+=gr._2.head.copy(qty = qty,weight = w, documents=docss)
                })

                buff.toList
              }.filter(s => s.code.startsWith(code))


              val rows = toRows(rn, in, materialNodes)
              //val filePath: String = Files.createTempDirectory("orderPdf").toAbsolutePath.toString + File.separator + dn.num + "_d" + new Date().getTime.toString + ".pdf"
              processPDF(dn, file.toString, rows)
            }
            case None => ""
          }
        }
        case None => ""
      }
    } else ""
    file.toString
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
        doc.showTextAligned(pa, mmToPt(204f + 210), mmToPt(14), i, TextAlignment.RIGHT, VerticalAlignment.TOP, 0)
      } else {
        val pa = new Paragraph(i.toString)
        pa.setFontSize(mmToPt(5))
        pa.setFont(fontHELVETICA)
        doc.showTextAligned(pa, mmToPt(200f + 210), mmToPt(14), i, TextAlignment.RIGHT, VerticalAlignment.TOP, 0)
      }
    })

    //GENERATE OUTLINES
    val root: PdfOutline = pdfDoc.getOutlines(false)
    var subRoot: PdfOutline = null
    descrTreeBuffer.foreach(descr => {
      val page = pdfDoc.getPage(descr.pageNum + 1)
      val dest: PdfExplicitDestination = PdfExplicitDestination.createFitH(page, descr.offset)
      val action = PdfAction.createGoTo(dest)
      if (descr.isRoot) {
        subRoot = root.addOutline(descr.A1 + " " + descr.A2)
        subRoot.addAction(action)
      } else {
        val sublocal = subRoot.addOutline(descr.A1 + " " + descr.A2)
        sublocal.addAction(action)
      }
    })
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
        setStampText(cellBuff(1), "МАТЕРИАЛ", italic = false, bold = true)
        setStampText(cellBuff(2), "ДОКУМЕНТ", italic = false, bold = true)
        setStampText(cellBuff(3), "Ед.Из.", italic = false, bold = true)
        setStampText(cellBuff(4), "НАЛИЧИЕ", italic = false, bold = true)
        setStampText(cellBuff(5), "КОЛ-ВО", italic = false, bold = true)

        setStampText(cellBuff(6), "", italic = false, bold = true)
        setStampText(cellBuff(7), "", italic = false, bold = true)
        setStampText(cellBuff(8), "ПРИМ", italic = false, bold = true)
        setStampText(cellBuff(9), "A", italic = false, bold = true)
        setStampText(cellBuff(10), "B", italic = false, bold = true)
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
        bodyGrid.addCell(generateCell(item.A2, TextAlignment.LEFT, 5))
        bodyGrid.addCell(generateCell(item.A3, TextAlignment.LEFT, 5))
        bodyGrid.addCell(generateCell(item.A4))
        bodyGrid.addCell(generateCell(item.A5))
        bodyGrid.addCell(generateCell(item.A6))

        bodyGrid.addCell(generateCell(""))
        bodyGrid.addCell(generateCell(""))
        bodyGrid.addCell(generateCell(item.A9, TextAlignment.LEFT, 5))
        bodyGrid.addCell(generateCell(""))
        bodyGrid.addCell(generateCell(item.A12))
      }
      else {
        bodyGrid.addCell(generateSpannedCellBold(item.A1))
        descrTreeBuffer += DescrTree(currPage, item.A1, "", item.A2.length == 6, currentRow * 5.0f)
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
    //val logo = getNnauticLigoEN.scaleToFit(72, 20).setFixedPosition(mmToPt(9), mmToPt(8))
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

  private def toRows(rootNode: MaterialNode, in: List[GlobalEsp], materialNodes: List[MaterialNode]): List[Item11ColumnsEN] = {
    val buff = ListBuffer.empty[Item11ColumnsEN]
    val ds = new DS(rootNode, in, materialNodes)
    ds.nextLevel().foreach(das1 => {
      if (!das1.rootNode.data.contains("XXX")) buff += Item11ColumnsEN(true, A1 = "(" + das1.rootNode.data + ")" + das1.rootNode.label_ru, A2 = das1.rootNode.data)
      das1.nextLevel().foreach(das2 => {
        if (!das2.rootNode.data.contains("XXX")) buff += Item11ColumnsEN(true, A1 = "(" + das2.rootNode.data + ")" + das2.rootNode.label_ru, A2 = das2.rootNode.data)
        das2.nextLevel().foreach(das3 => {
          val header = s"${das3.rootNode.label_ru} ${das2.rootNode.label_ru} ${das3.rootNode.data}"
          if (!das3.rootNode.data.contains("XXX")) buff += Item11ColumnsEN(true, A1 = header, A2 = das3.rootNode.data)
          das3.in.sortBy(b => b.code.takeRight(4)).foreach(d => {
            buff += Item11ColumnsEN(false, A1 = d.code.takeRight(4), A2 = d.name, A3 = d.material.description, A4 = d.units,
              A5 = "0", A6 = d.qty.toString, A7 = d.weight.toString, A8 = d.weightTotal.toString,
              A9 = d.material.provider, A10 = "", A11 = d.material.note)
          })
        })
      })
    })
    buff.toList
  }


}
