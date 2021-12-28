package local.pdf

import java.time.LocalDate
import java.time.format.DateTimeFormatter

trait UtilsPDF {


  def dateNow: String = LocalDate.now().format(DateTimeFormatter.ofPattern("dd.MM.yy"))

  def ptToMM(in: Double): Int = {
    Math.round((in + 2.0d) / 2.8d).toInt
  }

  def ptToMM(in: Float): Int = {
    Math.round((in + 2.0f) / 2.8f)
  }

  def mmToPt(in: Int): Float = {
    (in * 2.8346438836889).toFloat - 2
  }

  def mmToPt(in: Double): Float = {
    (in * 2.8346438836889).toFloat - 2
  }
}
