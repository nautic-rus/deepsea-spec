package local.hull.cnc.pellaESSI

import breeze.linalg.{DenseVector, norm}

import java.io.{File, PrintWriter}
import scala.Double.NaN
import scala.collection.mutable.ListBuffer

object EssiCNCManager {

  private val startOffset: Double = 60.0
  //private val arcRatio:Double=0.3
  private val arcRatio:Double=0.016
  //private val arcRadiiusRange=Tuple2(100.0,250000.0)

  private val arcRadiiusRange=Tuple2(290.0,800000.0)


  private val arcAlgoRadius: Boolean = false

  private val commands: List[String] = List[String]("CUTH", "CUT")

  private case class Point(x: Double, y: Double, offsetX: Double = 0.0, offsetY: Double = 0.0) {
    override def toString: String = s"${doubleToStr(x - offsetX)} ${doubleToStr(y - offsetY)}"

    def toESSI: String = {
      var ret = ""
      val xv = x - offsetX
      val yv = y - offsetY

      val strx = (Math.abs(xv)).toInt.toString
      val stry = (Math.abs(yv)).toInt.toString

      if(xv==0)ret += "+" else
      if (xv < 0) ret += "-" + strx else ret += "+" + strx

      if(yv==0)ret += "+" else
      if (yv < 0) ret += "-" + stry else ret += "+" + stry

      ret
    }

    //def toGcode(cmd: String, offsetCorrection:Point): String = s"${cmd}X${doubleToStr(x-offsetCorrection.x)}Y${doubleToStr(y-offsetCorrection.y)}"
  }

  private case class CNCcoordsPackage(num: Int, coords: List[Point])

  private case class CNC(name: String, A: Int = 0, B: Int = 0, machineItems: List[CNCcoordsPackage])

  private case class Arc(var sp: Point, var rotCenter: Point, var ep: Point) {
    override def toString: String = s"sp=${sp.toString} rp=${rotCenter.toString} ep=${ep.toString}"

    val radius: Double = Math.sqrt((rotCenter.x - sp.x) * (rotCenter.x - sp.x) + (rotCenter.y - sp.y) * (rotCenter.y - sp.y))


   // val radius2: Double = Math.sqrt((rotCenter.x - ep.x) * (rotCenter.x - ep.x) + (rotCenter.y - ep.y) * (rotCenter.y - ep.y))
    val vecEx: Double = ep.x - rotCenter.x
    val vecEy: Double = ep.y - rotCenter.y

    val v: DenseVector[Double] = DenseVector[Double](vecEx, vecEy)
    val centerVec: DenseVector[Double] = DenseVector[Double](rotCenter.x, rotCenter.y)
    val vNorm: Double = norm(v)
    val correctedPoint: DenseVector[Double] = centerVec + (radius * v) / vNorm

    //rotCenter = Point((Math.round(rotCenter.x/100.0))*100.0, (Math.round(rotCenter.y/100.0))*100.0)
    //sp = Point((Math.round(sp.x/100.0))*100.0, (Math.round(sp.y/100.0))*100.0)
    //ep = Point((Math.round(correctedPoint(0)/10.0))*10.0, (Math.round(correctedPoint(1)/10.0))*10.0)

    ep = Point((Math.round(correctedPoint(0))), (Math.round(correctedPoint(1))))


    //ep = Point(correctedPoint(0),correctedPoint(1))


    //val maxLen = Math.sqrt(radius * radius + radius * radius)
    def R: String = (radius).toInt.toString

    def isArc: Boolean = {
      val dx: Double =  ep.x-sp.x
      val dy =  ep.y-sp.y
      val n = norm(DenseVector[Double](dx, dy))

      val n2: Double = Math.sqrt((sp.x - ep.x) * (sp.x - ep.x) + (sp.y - ep.y) * (sp.y - ep.y))
      val rad=n/radius
      println(rad)
      //if (Math.abs(dx) + Math.abs(dy) > 0.1) true else false
      if (
       // (n > 180.0 && radius < 99999.0 && radius2 < 99999.0)

        rad>arcRatio && radius>arcRadiiusRange._1 && radius<arcRadiiusRange._2
          //&& rad <0.6

        //(n<180)  || (n<500 && radius >2400) || (radius > 99999.0 && radius2 > 99999.0)
          //n >180.0 && radius > 2000 && radius2 > 2000

      ) true else false
    }
  }

  private case class MachineItem(pointOrArc: Either[Point, Arc])

  private case class PseudoMachineOps(name: String, ops: List[MachineItem]) {
    override def toString: String = {
      var str: String = s"${name}\n"
      ops.foreach(op => {
        str += s"${op.toString}\n"
      })
      str
    }
  }


  def doEssiCNC(in: List[String], pathOut: String, username: String = "Misha"): List[String] = {

    val lineBuff = ListBuffer.empty[String]

    val offsetCorrection: Point = in.find(s => s.startsWith("STAR")) match {
      case Some(value) => {
        val arr = value.split(" ")
        if (arr.length == 3 && checkDigitOrPlusMinus(arr(1)) && checkDigitOrPlusMinus(arr(2))) {
          Point(arr(1).toDoubleOption.getOrElse(0.0d), arr(2).toDoubleOption.getOrElse(0.0d))
        } else {
          Point(0, 0)
        }
      }
      case None => Point(0, 0)
    }


    val plate: String = in.find(p => p.startsWith("PLAT")) match {
      case Some(value) => {
        val str = (value.replace("\n", "+")).split(" ")
        if (str.length == 3) {
          //"+"+str(1)+"0+"+str(2)+"0"
          "+100+100"
        } else {
          "+100+100"
        }
      }
      case None => "+100+100"
    }

    lineBuff += "39+20"


    val clp: List[CNC] = procForanCLPfromList(in, offsetCorrection)
    val machineOpts: List[PseudoMachineOps] = genPseudoMachineOps(clp)
    // val ret = doContour(machineOpts.head)
    val ret: List[String] = doContours(machineOpts)
    lineBuff ++= ret
    //val pw = new PrintWriter(new File(pathOut))
    //ret.foreach(line => pw.println(line))
    //pw.close()
    //val jj = 0

    lineBuff.toList
  }


  private def doubleToStr(in: Double): String = {
    val ret = ((in).toInt).toString
    if (ret.equals("-0")) "0" else ret
  }

  private def checkDigitOrPlusMinus(in: String): Boolean = {
    if (in.nonEmpty) {
      val c = in.trim.head
      if (c.isDigit) {
        true
      } else if (c.toString.equals("-")) {
        true
      } else if (c.toString.equals("+")) {
        true
      } else {
        false
      }


    } else {
      false
    }
  }

  private def procForanCLPfromList(lines: List[String], offset: Point): List[CNC] = {
    val buffLine = ListBuffer.empty[String]
    lines.foreach(l => buffLine += l)
    val tmpBuff = ListBuffer.empty[String]
    val concattedCommands = ListBuffer.empty[String]
    var readMode = false
    buffLine.foreach(l => {
      val cont = l.split(" ")
      if (!checkDigitOrPlusMinus(l)) readMode = commands.contains(cont.head)
      if (readMode) {
        if (cont.nonEmpty && commands.contains(cont.head)) {
          if (tmpBuff.nonEmpty) {
            concattedCommands += doCommand(tmpBuff.toList)
            tmpBuff.clear()
          }
          tmpBuff += l
        } else {
          if (tmpBuff.nonEmpty && checkDigitOrPlusMinus(l)) {
            tmpBuff += l
          }
        }
      }

    })
    if (tmpBuff.nonEmpty) concattedCommands += doCommand(tmpBuff.toList)
    tmpBuff.clear()
    val buff = ListBuffer.empty[CNC]
    concattedCommands.foreach(c => buff += doCNC(c, offset))
    buff.toList
  }

  private def doCommand(in: List[String]): String = {
    var str = ""
    in.foreach(l => str += l)
    str.replace("  ", " ").replace("   ", " ").trim
  }

  private def doubleListToPoints(in: List[Double], offset: Point): List[Point] = {
    val buffer = ListBuffer.empty[Point]
    (0 until in.length - 1 by 2).foreach(i => {

      val x: Double = Math.round(in(i)) * 10.0d
      val y = Math.round(in(i + 1)) * 10.0d
      val ox = Math.round(offset.x) * 10.0d
      val oy = Math.round(offset.x) * 10.0d

      //buffer += Point(in(i) * 10, in(i + 1) * 10, offset.x * 10, offset.y * 10)
      buffer += Point(x, y, ox, oy)
    })
    buffer.toList
  }

  private def doCNC(in: String, offset: Point): CNC = {

    val arr = in.split(" ")
    arr.head match {
      case "CUTH" | "CUT" => {
        val lb = ListBuffer.empty[CNCcoordsPackage]
        val A = arr(1).toIntOption.getOrElse(0)
        val B = 0
        val splitTogroup = in.split("999999 999999")
        val coords: List[Double] = {
          val buff = ListBuffer.empty[Double]
          splitTogroup.head.split(" ").drop(2).foreach(d => buff += d.toDoubleOption.getOrElse(0.0))
          buff.toList
        }

        lb += CNCcoordsPackage(1, doubleListToPoints(coords, offset))
        var counter = 2
        splitTogroup.tail.foreach(gr => {
          val buff = ListBuffer.empty[Double]
          gr.trim.split(" ").foreach(d => buff += d.toDoubleOption.getOrElse(0.0))
          lb += CNCcoordsPackage(counter, doubleListToPoints(buff.toList, offset))
          counter = counter + 1
        })
        CNC(arr.head, A, B, lb.toList)
      }
      case _ => CNC("ERROR " + arr.head, 0, 0, List[CNCcoordsPackage](CNCcoordsPackage(1, doubleListToPoints(List[Double](), offset))))
    }
  }


  private def genPseudoMachineOps(in: List[CNC]): List[PseudoMachineOps] = {
    val buff = ListBuffer.empty[PseudoMachineOps]
    in.foreach((op: CNC) => {
      val opBuff = ListBuffer.empty[MachineItem]

      op.machineItems.foreach((c: CNCcoordsPackage) => {
        opBuff ++= pointsToMachineItem(c.coords)
      })
      buff += PseudoMachineOps(op.name, opBuff.toList)
    })
    buff.toList
  }

  private def pointsToMachineItem(in: List[Point]): List[MachineItem] = {
    val buf = ListBuffer.empty[MachineItem]
    if (in.length == 1) {
      buf += MachineItem(Left(in.head))
    } else {
      val arcs = ListBuffer.empty[Arc]
      var p1: Point = Point(NaN, NaN)
      var p2: Point = Point(NaN, NaN)
      arcs += Arc(in(0), in(1), in(2))
      in.drop(3).foreach(p => {
        if (p1.x.isNaN && p1.y.isNaN && p2.x.isNaN && p2.y.isNaN) {
          p1 = arcs.toList.last.ep
          p2 = p
        } else {
          arcs += Arc(p1, p2, p)
          p1 = Point(NaN, NaN)
          p2 = Point(NaN, NaN)
        }
      })

      arcs.foreach(arc => {
        if (arc.isArc) {
          buf += MachineItem(Right(arc))
        } else {
          buf += MachineItem(Left(arc.sp))
          buf += MachineItem(Left(arc.ep))
        }
      })
    }
    buf.toList
  }

  private def isClockWise(arc: Arc): Boolean = {
    val xRot: Double = arc.rotCenter.x
    val yRot: Double = arc.rotCenter.y
    val xStrart: Double = arc.sp.x
    val yStart: Double = arc.sp.y
    val xEnd: Double = arc.ep.x
    val yEnd: Double = arc.ep.y
    (xStrart - xRot) * (yEnd - yRot) - (yStart - yRot) * (xEnd - xRot) > 0
  }

  private def doContours(inList: List[PseudoMachineOps]): List[String] = {
    val buff = ListBuffer.empty[String]
    var currPos: Point = Point(10, 10)
    inList.foreach(contour => {
      val freeMovePoint: Point = {
        contour.ops.head.pointOrArc match {
          case Right(value: Arc) => value.sp
          case Left(value: Point) => value
        }
      }

      val freeMovePointEP: Point = {
        contour.ops.head.pointOrArc match {
          case Right(value: Arc) => value.ep
          case Left(value: Point) => {
            contour.ops(1).pointOrArc match {
              case Right(value: Arc) => value.sp
              case Left(value: Point) => value
            }
          }
        }
      }

      val pOffset = cutOffseCalc(freeMovePoint, freeMovePointEP)

      val dx = pOffset.x - currPos.x
      val dy = pOffset.y - currPos.y
      if ((Math.abs(dx) + Math.abs(dy))/2 > 10) {
        buff += Point(dx, dy).toESSI
        currPos = pOffset
      }

      buff += "+5+5"
      buff += "7"
      buff += "29" //left compensation 30//rightcompensation


      contour.ops.foreach(op => {

        op.pointOrArc match {
          case Right(arc: Arc) => {
            //val r = arc.R
            val dir = if (isClockWise(arc)) "+" else "-"



            val dx = arc.sp.x - currPos.x
            val dy = arc.sp.y - currPos.y
            if ((Math.abs(dx) + Math.abs(dy))/2 > 10) {
              buff += Point(dx, dy).toESSI
              currPos = arc.sp
            }
            val arcdx = arc.ep.x - currPos.x
            val arcdy = arc.ep.y - currPos.y

            val RCX = arc.rotCenter.x - currPos.x
            val RCY = arc.rotCenter.y - currPos.y

            buff += Point(arcdx, arcdy).toESSI + Point(RCX, RCY).toESSI + dir
            currPos = arc.ep
          }
          case Left(p: Point) => {
            val dx = p.x - currPos.x
            val dy = p.y - currPos.y
            if ((Math.abs(dx) + Math.abs(dy))/2 > 10) {
              buff += Point(dx, dy).toESSI
              currPos = p
            }
          }
        }

      })

      /*      currPos = {
              contour.ops.last.pointOrArc match {
                case Right(value: Arc) => value.sp
                case Left(value: Point) => value
              }
            }*/

      buff += "8"
    })
    buff += "38"
    buff += "63"
    buff.toList
  }

  private def cutOffseCalc(sp: Point, ep: Point, isOuter: Boolean = false): Point = {


    val spX: Double = sp.x
    val spY: Double = sp.y
    val epX: Double = ep.x
    val epY: Double = ep.y
    val dist = startOffset
    val vX = epX - spX
    val vY = epY - spY
    val v: DenseVector[Double] = DenseVector[Double](vX, vY)
    val vNorm: Double = norm(v)
    val angle = Math.PI / 2.0
    val xRotated = spX + dist * ((vX * Math.cos(angle) - vY * Math.sin(angle)) / vNorm)
    val yRotated = spY + dist * ((vX * Math.sin(angle) + vY * Math.cos(angle)) / vNorm)
    Point(xRotated, yRotated)
  }

  private def doContoursOld(inList: List[PseudoMachineOps]): List[String] = {
    val buff = ListBuffer.empty[String]
    var currPos: Point = Point(0, 0)
    inList.foreach(contour => {
      val freeMovePoint: Point = {
        contour.ops.head.pointOrArc match {
          case Right(value: Arc) => value.sp
          case Left(value: Point) => value
        }
      }
      if (currPos.x == 0 && currPos.y == 0) {
        buff += freeMovePoint.toESSI
        currPos = freeMovePoint
      } else {
        val dx = currPos.x - freeMovePoint.x
        val dy = currPos.y - freeMovePoint.y
        if (Math.abs(dx) + Math.abs(dy) > 0.01) {
          buff += Point(dx, dy).toESSI
          currPos = freeMovePoint
        }
      }
      buff += "29" //left compensation 30//rightcompensation
      buff += "39+25"
      buff += "7"
      contour.ops.foreach(op => {
        op.pointOrArc match {
          case Right(arc: Arc) => {
            val r = arc.R
            val dir = if (isClockWise(arc)) "+" else "-"
            val s = "-"

            val dx = currPos.x - arc.sp.x
            val dy = currPos.y - arc.sp.y
            if (Math.abs(dx) + Math.abs(dy) > 0.01) {
              buff += Point(dx, dy).toESSI
              currPos = arc.sp
            }
            val arcdx = currPos.x - arc.ep.x
            val arcdy = currPos.y - arc.ep.y

            val RCX = currPos.x - arc.rotCenter.x
            val RCY = currPos.y - arc.rotCenter.y

            buff += Point(arcdx, arcdy).toESSI + Point(RCX, RCY).toESSI + dir
            currPos = arc.ep
          }
          case Left(p: Point) => {
            val dx = currPos.x - p.x
            val dy = currPos.y - p.y
            if (Math.abs(dx) + Math.abs(dy) > 0.01) {
              buff += Point(dx, dy).toESSI
              currPos = p
            }
          }
        }

      })
      buff += "8"
    })
    buff += "63"
    buff.toList
  }

  private def doContour(in: PseudoMachineOps): List[String] = {
    val buff = ListBuffer.empty[String]

    var currPos: Point = in.ops.head.pointOrArc match {
      case Right(value: Arc) => value.sp
      case Left(value: Point) => value
    }

    buff += currPos.toESSI

    in.ops.foreach(op => {
      op.pointOrArc match {
        case Right(arc: Arc) => {
          val r = arc.R
          val dir = if (isClockWise(arc)) "+" else "-"
          val s = "-"

          val dx = currPos.x - arc.sp.x
          val dy = currPos.y - arc.sp.y
          if (Math.abs(dx) + Math.abs(dy) > 0.01) {
            buff += Point(dx, dy).toESSI
            currPos = arc.sp
          }
          val arcdx = currPos.x - arc.ep.x
          val arcdy = currPos.y - arc.ep.y

          val RCX = currPos.x - arc.rotCenter.x
          val RCY = currPos.y - arc.rotCenter.y

          buff += Point(arcdx, arcdy).toESSI + Point(RCX, RCY).toESSI + dir
          currPos = arc.ep
        }
        case Left(p: Point) => {
          val dx = currPos.x - p.x
          val dy = currPos.y - p.y
          if (Math.abs(dx) + Math.abs(dy) > 0.01) {
            buff += Point(dx, dy).toESSI
            currPos = p
          }
        }
      }

    })


    buff.toList
  }


}
