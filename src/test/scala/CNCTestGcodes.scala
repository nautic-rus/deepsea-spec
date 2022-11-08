import local.hull.cnc.hyprethermGcode.CNCManager.doCNCStrings
import org.apache.commons.io.FilenameUtils
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, PrintWriter}
import scala.io.{BufferedSource, Source}

class CNCTestGcodes extends AnyFunSuite {

/*  val files: List[File] = getListOfFiles("c:\\32\\")

  files.foreach(f => {

    val filename=FilenameUtils.getBaseName(f.getName)

    val src: BufferedSource = Source.fromFile(f)
    val lines: List[String] = src.getLines.toList
    src.close()

    val retStar = doCNCStrings(lines, "Misha")
    val pw = new PrintWriter(new File("c:\\11\\" + filename + ".mpg"))
    retStar.foreach(line => pw.println(line))
    pw.close()

  })

  val jj=0*/

  def getListOfFiles(dir: String): List[File] = {
    val d = new File(dir)
    if (d.exists && d.isDirectory) {
      d.listFiles.filter(_.isFile).toList
    } else {
      List[File]()
    }
  }



  val listIn: List[String] = List(
    "U406-14"
  )

  listIn.foreach(f=>{
  val src: BufferedSource = Source.fromFile("c:\\35\\"+f+".txt")
  //val src: BufferedSource = Source.fromFile("c:\\32\\U306_11_0.txt")
  val lines: List[String] = src.getLines.toList
  src.close()
  //val ret: String = doCNC(lines, "c:\\26\\C-N004-ULTT-01"+f+".mpg", "Misha")
  val retStar = doCNCStrings(lines, "Misha")
  val pw = new PrintWriter(new File("c:\\35\\A\\"+f+".mpg"))
  retStar.foreach(line => pw.println(line))
  pw.close()

  })
}
