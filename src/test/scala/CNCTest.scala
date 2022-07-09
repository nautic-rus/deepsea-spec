
import local.hull.cnc.pellaESSI.EssiCNCManager.doEssiCNC
import org.scalatest.funsuite.AnyFunSuite

import java.io.{File, PrintWriter}
import scala.io.{BufferedSource, Source}

class CNCTest extends AnyFunSuite {

  val listIn: List[String] =List(
    "C-N004-ULTT-01",
    "C-N004-ULTT-02",
    "C-N004-ULTT-03",
    "C-N004-ULTT-04",
    "C-N004-ULTT-05",
    "C-N004-ULTT-06",
    "C-N004-ULTT-07",
    "C-N004-ULTT-08",
    "C-N004-ULTT-09",
    "C-N004-ULTT-10",
    "C-N004-ULTT-11",


    "C-N004-U302-01",
    "C-N004-U302-02",
    "C-N004-U302-03",
    "C-N004-U302-04",
    "C-N004-U302-05",
    "C-N004-U302-06",
    "C-N004-U302-07",
    "C-N004-U302-08",
    "C-N004-U302-09",
    "C-N004-U302-10",
    "C-N004-U302-11",
    "C-N004-U302-12",
    "C-N004-U302-13",
    "C-N004-U302-14",
    "C-N004-U302-15",
    "C-N004-U302-16",
    "C-N004-U302-17",
    "C-N004-U302-18",
    "C-N004-U302-19",
    "C-N004-U302-20",
  )


  val listInOne: List[String] =List(
    "C-N004-U302-19"
  )
  listIn.foreach(f=>{

    val src: BufferedSource = Source.fromFile("c:\\27\\demo\\"+f+".txt")
    val lines: List[String] = src.getLines.toList
    src.close()
    val retStar=doEssiCNC(lines, "", "Misha")
    val pw = new PrintWriter(new File("c:\\27\\demoESSI\\"+f+".esi"))
    retStar.foreach(line=>pw.println(line))
    pw.close()
  })


/*  val src: BufferedSource = Source.fromFile("c:\\27\\C-N004-ULTT-02.txt")
  val lines: List[String] = src.getLines.toList
  src.close()
  val ret = doEssiCNC(lines, "c:\\27\\demo.esi", "Misha")
  */


 // val retStar=doCNCStrings(lines,"Misha")
 // val pw = new PrintWriter(new File("c:\\27\\C-N004-ULTT-02.esi"))
 // retStar.foreach(line=>pw.println(line))
  //pw.close()


/*  val listIn: List[String] =List(
    "C-N004-ULTT-01",
    "C-N004-ULTT-02",
    "C-N004-ULTT-03",
    "C-N004-ULTT-04",
    "C-N004-ULTT-05",
    "C-N004-ULTT-06",
    "C-N004-ULTT-07",
    "C-N004-ULTT-08",
    "C-N004-ULTT-09",
    "C-N004-ULTT-10",
    "C-N004-ULTT-11",
  )

  listIn.foreach(f=>{
    val src: BufferedSource = Source.fromFile("c:\\26\\"+f+".txt")
    val lines: List[String] = src.getLines.toList
    src.close()
    //val ret: String = doCNC(lines, "c:\\26\\C-N004-ULTT-01"+f+".mpg", "Misha")
    val retStar=doCNCStrings(lines,"Misha")
    val pw = new PrintWriter(new File("c:\\26\\C-N004-ULTT-01"+f+".mpg"))
    retStar.foreach(line=>pw.println(line))
    pw.close()

  })*/

/*  val src: BufferedSource = Source.fromFile("c:\\26\\C-N004-ULTT-01.txt")
  val lines: List[String] = src.getLines.toList
  src.close()
  val ret: String = doCNC(lines, "c:\\26\\C-N004-ULTT-01.mpg", "Misha")*/



  val jj = 0
}
