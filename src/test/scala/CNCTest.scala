
import breeze.linalg
import breeze.linalg.{DenseVector, norm}


//import local.hull.cnc.pellaESSI.EssiCNCManager.doEssiCNC
import local.hull.cnc.pellaESSI.EssiCNCManagerSubdiv.doEssiCNC


import org.scalatest.funsuite.AnyFunSuite
import spire.std.ArraySupport.dot

import java.io.{File, PrintWriter}
import scala.io.{BufferedSource, Source}

class CNCTest extends AnyFunSuite {

  /*  val spX=50.0
    val spY=50.0
    val rpX=0.0
    val rpY= -50.0
    val epX=107.9938
    val epY= -21.0632*/

  /*  val epX= -100.0
    val epY= 0.0

    val spX= -28.9368
    val spY= 57.9938


    val rpX=0.0
    val rpY= -50.0





    val v1=DenseVector[Double]( epX - rpX, epY - rpY)
    val v2=DenseVector[Double]( spX - rpX, spY - rpY)

    val midP=DenseVector[Double]( (spX + epX)/2.0, (spY + epY)/2.0)

    val v3=DenseVector[Double](midP(0) - rpX, midP(1) - rpY)



    calcBulge(v1,v2, v3)

    def calcBulge(v1:DenseVector[Double], v2:DenseVector[Double], v3:DenseVector[Double]):Double={

      val hord=DenseVector[Double]( v1(0) - v2(0), v1(1) - v2(1))

      val len=norm(hord)
      val len2=norm(v3)

      val r1=norm(v1)
      val r2=norm(v2)
      val r=(r1+r2)/2

      val bulge=Math.abs(r-len2)




      val midPoint=DenseVector[Double]( (v1(0) - v2(0))/2.0, (v1(1) - v2(1))/2)








      0.0
    }*/


  /*
    val v: DenseVector[Double] = DenseVector[Double](-10, 10)
    val vNorm: Double = norm(v)

    val ox: DenseVector[Double] = DenseVector[Double](1, 0)
    val oy: DenseVector[Double] = DenseVector[Double](0, 1)

    val cosX: Double = Math.max(Math.min((v dot ox) / (vNorm * norm(ox)), 1), -1)
    val cosY: Double = Math.max(Math.min((v dot oy) / (vNorm * norm(oy)), 1), -1)

    val angle = -Math.PI / 2.0

    val xRotated = (v(0) * Math.cos(angle) - v(1) * Math.sin(angle)) / vNorm
    val yRotated = (v(0) * Math.sin(angle) + v(1) * Math.cos(angle)) / vNorm


    cutOffseCalc(10, 10, 20, 20)

    //x1 = x0 + 0.1 * vx / size(vector) ---xRotated
    //y1 = y0 + 0.1 * vy / size(vector) ---yRotated


    def cutOffseCalc(spX: Double, spY: Double, epX: Double, epY: Double): Unit = {
      val dist = 6
      val vX = epX - spX
      val vY = epY - spY
      val angle = Math.PI / 2.0
      val xRotated = spX + dist * ((vX * Math.cos(angle) - vY * Math.sin(angle)) / vNorm)
      val yRotated = spY + dist * ((vX * Math.sin(angle) + vY * Math.cos(angle)) / vNorm)
    }


    def cutOffseCalc2(spX: Double, spY: Double, epX: Double, epY: Double): Unit = {

      val dist = 6

      val v: DenseVector[Double] = DenseVector[Double](epX - spX, epY - spY)
      val ox: DenseVector[Double] = DenseVector[Double](1, 0)
      val oy: DenseVector[Double] = DenseVector[Double](0, 1)

      val cosX: Double = Math.max(Math.min((v dot ox) / (vNorm * norm(ox)), 1), -1)
      val cosY: Double = Math.max(Math.min((v dot oy) / (vNorm * norm(oy)), 1), -1)

      val angle = Math.PI / 2.0

      val xRotated = spX + dist * ((v(0) * Math.cos(angle) - v(1) * Math.sin(angle)) / vNorm)
      val yRotated = spY + dist * ((v(0) * Math.sin(angle) + v(1) * Math.cos(angle)) / vNorm)

      val jj = 0


    }

  */

  val listIn: List[String] = List(


/*    "U0105_T1",
    "U0105_T2",*/

    "U0105-01",
    "U0105-02",
    "U0105-03",
    "U0105-04",
    "U0105-05",
    "U0105-06",
    "U0105-07",
    "U0105-08",
    "U0105-09",
    "U0105-10",

    "U0105-11",
    "U0105-12",
    "U0105-13",
    "U0105-14",
    "U0105-15",
    "U0105-16",
    "U0105-17",
    "U0105-18",
    "U0105-19",
    "U0105-20",

    "U0105-21",
    "U0105-22",
    "U0105-23",
    "U0105-24",
    "U0105-25",
    "U0105-26",
    "U0105-27",

    "U0105-30",

    "U0105-31",
    "U0105-32",
    "U0105-33",
    "U0105-34",
    "U0105-35",
    "U0105-36",
    "U0105-37",
    "U0105-38",
    "U0105-39",
    "U0105-40",




    "U0104-19"




  )


  /*  val retStar: List[String]=doEssiCNCTestData()
    val pw = new PrintWriter(new File("c:\\27\\demoESSI105\\" + "testdata" + ".esi"))
    retStar.foreach(line => pw.println(line))
    pw.close()
    val jjs=0*/

  //c:\27\104\U0104_25_0.txt

/*  val src: BufferedSource = Source.fromFile("c:\\27\\104\\U0104_33_0.txt")
  val lines: List[String] = src.getLines.toList
  src.close()
  val retStar: List[String] = doEssiCNC(lines, "Misha")
  val pw = new PrintWriter(new File("c:\\27\\104\\" + "U0104_33_0.esi"))
  retStar.foreach(line => pw.println(line))
  pw.close()

  val h=0*/


  listIn.foreach(f => {

    val src: BufferedSource = Source.fromFile("c:\\27\\105\\" + f + ".txt")
    val lines: List[String] = src.getLines.toList
    src.close()
    val retStar: List[String] = doEssiCNC(lines,  "Misha")
    val pw = new PrintWriter(new File("c:\\27\\ESSI105\\" + f + ".esi"))
    retStar.foreach(line => pw.println(line))
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
