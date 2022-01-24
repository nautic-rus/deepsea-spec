package local.pdf.ru.common

import com.itextpdf.io.font.{FontProgramFactory, PdfEncodings}
import com.itextpdf.io.image.{ImageData, ImageDataFactory}
import com.itextpdf.kernel.font.{PdfFont, PdfFontFactory}
import com.itextpdf.kernel.font.PdfFontFactory.EmbeddingStrategy
import com.itextpdf.kernel.pdf.PdfPage
import com.itextpdf.kernel.pdf.canvas.PdfCanvas
import com.itextpdf.layout.element.{Cell, Image, Paragraph, Table, Text}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment, VerticalAlignment}
import local.pdf.UtilsPDF

import scala.jdk.CollectionConverters._
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.collection.mutable.ListBuffer

object ReportCommon extends UtilsPDF {
  case class DocName(code: String = "XXXXX", num: String = "170701-XXX-XXXX", name: String = "XXXXXXXXXXX",
                     lastRev: String = "rev.0", userDev: String = "Голенищев", userTCheck: String = "Воронин",
                     userNCheck: String = "Воронин", userAgree: String = "Стропилов")

  case class Item11Columns(isHeader: Boolean = false, A1: String, A2: String = "", A3: String = "", A4: String = "", A5: String = "", A6: String = "", A7: String = "", A8: String = "", A9: String = "", A10: String = "", A11: String = "", project: String = "", A12: String = "")

  case class ChangedItem11Columns(before: Item11Columns, now: Item11Columns)

  val cuurrentCompany: String = "Наутик-Рус"

  def gostFont: PdfFont = PdfFontFactory.createFont(FontProgramFactory.createFont("src/main/resources/fonts/GOSTtypeA.ttf"), PdfEncodings.IDENTITY_H, EmbeddingStrategy.PREFER_NOT_EMBEDDED)

  def getNnauticLigo: Image = {
    val imageData: ImageData = ImageDataFactory.create("src/main/resources/pict/nrlogo.png")
    new Image(imageData)
  }

  def genBaseStampBig(data: DocName, debug: Boolean = false): Table = {
    val defaultFontSize = mmToPt(4.1)
    val cellBuff = ListBuffer.empty[Cell]
    val pointColumnWidths = Array(
      mmToPt(7), mmToPt(10), mmToPt(23),
      mmToPt(15), mmToPt(10), mmToPt(70),
      mmToPt(5), mmToPt(5), mmToPt(5), mmToPt(5),
      mmToPt(12), mmToPt(18)
    )
    val table = new Table(pointColumnWidths)
    val tableWidthPt = mmToPt(185)
    val tableHeightPt = mmToPt(55)
    table.setWidth(tableWidthPt)
    table.setHeight(tableHeightPt)
    table.setHorizontalAlignment(HorizontalAlignment.CENTER)
    table.setVerticalAlignment(VerticalAlignment.MIDDLE)

    def generateRows(): Unit = {
      val defaultFontSize = mmToPt(4.1)

      //R1
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
        new Cell(3, 7)
          .setVerticalAlignment(VerticalAlignment.MIDDLE)
          .setHorizontalAlignment(HorizontalAlignment.CENTER)
          .add(new Paragraph("")
            .setTextAlignment(TextAlignment.CENTER)
            .setFontSize(mmToPt(10))
            .setFont(gostFont)
          )
          .setPadding(0).setMargin(0)
          .setHeight(mmToPt(15))
          .setWidth(mmToPt(100))
      }
      //R2
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
      //R3
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


      //R4
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
        new Cell(0, 0).setVerticalAlignment(VerticalAlignment.MIDDLE)
          .setHorizontalAlignment(HorizontalAlignment.CENTER)
          .add(new Paragraph("")
            .setTextAlignment(TextAlignment.CENTER)
            .setFontSize(defaultFontSize)
            .setFont(gostFont)
          )
          .setPadding(0).setMargin(0)
          .setHeight(mmToPt(5))
      }
      //21
      cellBuff += {
        new Cell(5, 0)
          .setVerticalAlignment(VerticalAlignment.MIDDLE)
          .setHorizontalAlignment(HorizontalAlignment.CENTER)
          .add(new Paragraph("")
            .setTextAlignment(TextAlignment.CENTER)
            .setFontSize(mmToPt(6))
            .setFont(gostFont)

          )
          .setPadding(0).setMargin(0)
          .setHeight(mmToPt(25))
          .setWidth(mmToPt(50))
      }
      cellBuff += {
        new Cell(0, 3)
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

      //R5
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
        new Cell(3, 2)
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

      //r6
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

      //r7
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

      //r8
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
        new Cell(0, 4)
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

      //r9
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
        new Cell(3, 6)
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


      //r10
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

      //r11
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
      if (debug) {
        cell.getChildren.asScala.head.asInstanceOf[Paragraph].getChildren.asScala.head.asInstanceOf[Text]
          .setText(cellBuff.indexOf(cell).toString)
      }
      table.addCell(cell)
    })

    def processStaticText(): Unit = {
      setStampText(cellBuff(25), "Изм", italic = true)
      setStampText(cellBuff(26), "Лист", italic = true)
      setStampText(cellBuff(27), "№ докум.", italic = true)
      setStampText(cellBuff(28), "Подпись", italic = true)
      setStampText(cellBuff(29), "Дата", italic = true)

      setStampText(cellBuff(35), "Разраб.", italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(39), "Пров.", italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(43), "Т.контр.", italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(55), "Н.контр.", italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(59), "Утв.", italic = true, textAlignment = TextAlignment.LEFT)

      setStampText(cellBuff(22), "Лит", italic = true)
      setStampText(cellBuff(23), "Масса", italic = true)
      setStampText(cellBuff(24), "Масштаб", italic = true)
      setStampText(cellBuff(47), "Лист 1", italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(48), "Листов", italic = true, textAlignment = TextAlignment.LEFT)

      setStampText(cellBuff(20), dateNow, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(36), data.userDev, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(40), data.userNCheck, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(44), data.userTCheck, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(56), data.userAgree, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(60), data.userAgree, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(38), dateNow, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(42), dateNow, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(46), dateNow, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(58), dateNow, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(62), dateNow, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(16), data.lastRev, italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(5), data.num, italic = true, textAlignment = TextAlignment.CENTER, bold = true, fontSize = 15.0f)
      setStampText(cellBuff(21), data.name, italic = true, textAlignment = TextAlignment.CENTER, bold = false, fontSize = 8.0f)
      //setStampText(cellBuff(54), cuurrentCompany, italic = true, textAlignment = TextAlignment.CENTER, bold = true, fontSize = 8.0f)
    }

    processStaticText()
    table
  }

  def genBaseStampSmall(data: DocName, debug: Boolean = false): Table = {
    val cellBuff = ListBuffer.empty[Cell]
    val pointColumnWidths = Array(mmToPt(7), mmToPt(10), mmToPt(23),
      mmToPt(15), mmToPt(10), mmToPt(110), mmToPt(10)
    )
    val table = new Table(pointColumnWidths)
    val tableWidthPt = mmToPt(185)
    val tableHeightPt = mmToPt(15)
    table.setWidth(tableWidthPt)
    table.setHeight(tableHeightPt)
    table.setHorizontalAlignment(HorizontalAlignment.CENTER)
    table.setVerticalAlignment(VerticalAlignment.MIDDLE)


    def generateRows(): Unit = {
      val defaultFontSize = mmToPt(4.1)
      //R1
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
        new Cell(3, 0)
          .setVerticalAlignment(VerticalAlignment.MIDDLE)
          .setHorizontalAlignment(HorizontalAlignment.CENTER)
          .add(new Paragraph("")
            .setTextAlignment(TextAlignment.CENTER)
            .setFontSize(mmToPt(10))
            .setFont(gostFont)
          )
          .setPadding(0).setMargin(0)
          .setHeight(mmToPt(15))
          .setWidth(mmToPt(150)
          )
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

      //r2
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

      //r3
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

    def processStaticText(): Unit = {
      setStampText(cellBuff(13), "Изм", italic = true)
      setStampText(cellBuff(14), "Лист", italic = true)
      setStampText(cellBuff(15), "№ докум.", italic = true)
      setStampText(cellBuff(16), "Подпись", italic = true)
      setStampText(cellBuff(17), "Дата", italic = true)
      setStampText(cellBuff(6), "Лист", italic = true, textAlignment = TextAlignment.LEFT)
      setStampText(cellBuff(5), data.num, italic = true, textAlignment = TextAlignment.CENTER, bold = true, fontSize = 12)
      setStampText(cellBuff(7), data.lastRev, italic = true)
      setStampText(cellBuff(11), dateNow, italic = true)
    }

    processStaticText()
    table
  }

  def borderESKD(pdfPage: PdfPage): Unit = {
    val canvas = new PdfCanvas(pdfPage)
    canvas.rectangle(mmToPt(20), mmToPt(5), pdfPage.getDocument.getDefaultPageSize.getWidth - mmToPt(25), pdfPage.getDocument.getDefaultPageSize.getHeight - mmToPt(10))
    canvas.stroke()
  }

  private def setStampText(cell: Cell, text: String, italic: Boolean = false, bold: Boolean = false, textAlignment: TextAlignment = TextAlignment.CENTER, fontSize: Float = 4.1f): Cell = {
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


}
