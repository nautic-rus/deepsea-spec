
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

    "U0105_T1",
    "U0105_T2",

    "U0105_01",
    "U0105_02",
    "U0105_03",
    "U0105_04",
    "U0105_05",
    "U0105_06",
    "U0105_07",
    "U0105_08",
    "U0105_09",
    "U0105_10",

    "U0105_11",
    "U0105_12",
    "U0105_13",
    "U0105_14",
    "U0105_15",
    "U0105_16",
    "U0105_17",
    "U0105_18",
    "U0105_19",
    "U0105_20",

    "U0105_21",
    "U0105_22",
    "U0105_23",
    "U0105_24",
    "U0105_25",
    "U0105_26",
    "U0105_27",

    "U0105_30",

    "U0105_31",
    "U0105_32",
    "U0105_33",
    "U0105_34",
    "U0105_35",
    "U0105_36",
    "U0105_37",
    "U0105_38",
    "U0105_39",
    "U0105_40",
    "U0105_41",


  )


  /*  val retStar: List[String]=doEssiCNCTestData()
    val pw = new PrintWriter(new File("c:\\27\\demoESSI105\\" + "testdata" + ".esi"))
    retStar.foreach(line => pw.println(line))
    pw.close()
    val jjs=0*/


  listIn.foreach(f => {

    val src: BufferedSource = Source.fromFile("c:\\27\\105\\" + f + "_0.txt")
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
