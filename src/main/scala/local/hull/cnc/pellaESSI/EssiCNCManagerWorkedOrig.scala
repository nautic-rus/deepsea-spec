package local.hull.cnc.pellaESSI

import breeze.linalg.{DenseVector, norm}

import scala.Double.NaN
import scala.collection.mutable.ListBuffer

object EssiCNCManagerWorkedOrig {
  private var offset: Double = 40.0
  private val startOffset: Double = 60.0
  private val minArcBugle: Double = 16 //14

  private val commands: List[String] = List[String]("CUTH", "CUT")

  private case class Point(x: Double, y: Double, offsetX: Double = 0.0, offsetY: Double = 0.0) {
    override def toString: String = s"${doubleToStr(x - offsetX)} ${doubleToStr(y - offsetY)}"

    def toESSI: String = {
      var ret = ""
      val xv = x - offsetX
      val yv = y - offsetY

      val strx = (Math.abs(xv)).toInt.toString
      val stry = (Math.abs(yv)).toInt.toString

      if (strx.equals("0")) ret += "+" else if (xv < 0) ret += "-" + strx else ret += "+" + strx

      if (stry.equals("0")) ret += "+" else if (yv < 0) ret += "-" + stry else ret += "+" + stry

      ret
    }

    //def toGcode(cmd: String, offsetCorrection:Point): String = s"${cmd}X${doubleToStr(x-offsetCorrection.x)}Y${doubleToStr(y-offsetCorrection.y)}"
  }

  private case class CNCcoordsPackage(num: Int, coords: List[Point])

  private case class CNC(name: String, A: Int = 0, B: Int = 0, machineItems: List[CNCcoordsPackage])

  private case class Arc(var sp: Point, var rotCenter: Point, var ep: Point) {

    val midP: Point = Point((sp.x + ep.x) / 2.0, (sp.y + ep.y) / 2.0)
    val midPoint: DenseVector[Double] = DenseVector[Double](midP.x, midP.y)
    val midPCenterVector: DenseVector[Double] = DenseVector[Double](rotCenter.x - midP.x, rotCenter.y - midP.y)
    val midPCenterVectorLen: Double = norm(midPCenterVector)

    rotCenter = {
      val hordVector: DenseVector[Double] = DenseVector[Double]((ep.x - sp.x), (ep.y - sp.y))
      val hordVectorLen: Double = norm(hordVector)
      val hordVectorDir: DenseVector[Double] = hordVector / hordVectorLen

      val hordVectorMove: DenseVector[Double] = {
        val hordVectorDirCW90: DenseVector[Double] = DenseVector[Double](hordVectorDir(1) * -1, hordVectorDir(0))
        val hordVectorDirACW90: DenseVector[Double] = DenseVector[Double](hordVectorDir(1), hordVectorDir(0) * -1)

        val angleA: Double = Math.abs((Math.max(Math.min((hordVectorDirCW90 dot midPCenterVector) / (norm(hordVectorDirCW90) * norm(midPCenterVector)), 1), -1)))
        val angleB: Double = Math.abs((Math.max(Math.min((hordVectorDirACW90 dot midPCenterVector) / (norm(hordVectorDirACW90) * norm(midPCenterVector)), 1), -1)))

        if (angleA <= angleB) hordVectorDirCW90 else hordVectorDirACW90
      }

      val correctedCenterPoint: DenseVector[Double] = midPoint + (midPCenterVectorLen * hordVectorMove)
      Point(correctedCenterPoint(0), correctedCenterPoint(1))
    }

    override def toString: String = s"sp=${sp.toString} rp=${rotCenter.toString} ep=${ep.toString}"

    val originalEP: Point = ep

    val radius: Double = Math.sqrt((rotCenter.x - sp.x) * (rotCenter.x - sp.x) + (rotCenter.y - sp.y) * (rotCenter.y - sp.y))
    val radius2: Double = Math.sqrt((rotCenter.x - ep.x) * (rotCenter.x - ep.x) + (rotCenter.y - ep.y) * (rotCenter.y - ep.y))

    val vecEx: Double = ep.x - rotCenter.x
    val vecEy: Double = ep.y - rotCenter.y

    val vesSx: Double = sp.x - rotCenter.x
    val vecSy: Double = sp.y - rotCenter.y

    val OS: DenseVector[Double] = DenseVector[Double](vesSx, vecSy)
    val OE: DenseVector[Double] = DenseVector[Double](vecEx, vecEy)
    val angle: Double = Math.acos(Math.max(Math.min((OS dot OE) / (norm(OS) * norm(OE)), 1), -1))


    val v: DenseVector[Double] = DenseVector[Double](vecEx, vecEy)
    val centerVec: DenseVector[Double] = DenseVector[Double](rotCenter.x, rotCenter.y)
    val vNorm: Double = norm(v)
    val correctedPoint: DenseVector[Double] = centerVec + (radius * v) / vNorm

    //rotCenter = Point((Math.round(rotCenter.x/100.0))*100.0, (Math.round(rotCenter.y/100.0))*100.0)
    //sp = Point((Math.round(sp.x/100.0))*100.0, (Math.round(sp.y/100.0))*100.0)
    //ep = Point((Math.round(correctedPoint(0)/10.0))*10.0, (Math.round(correctedPoint(1)/10.0))*10.0)
    //ep = Point((Math.round(correctedPoint(0))), (Math.round(correctedPoint(1))))
    //println(eporig.toString+"  "+ep.toString +" R "+ (radius-radius2).toString)


    /*    (x, y) rotated 90 degrees around(0, 0) is (-y, x). If you want to rotate clockwise , you simply do it the other way around, getting(y, -x)*/

    val bulge: Double = {
      val r = (radius + radius2) / 2
      Math.abs(r - midPCenterVectorLen)
    }
    /*    val bulge: Double = {
          val r = (radius + radius2) / 2
          Math.abs(r - midPCenterVectorLen)
        }*/

    //ep = Point(correctedPoint(0), correctedPoint(1))

    //val maxLen = Math.sqrt(radius * radius + radius * radius)
    def R: String = (radius).toInt.toString

    def isArc: Boolean = {
      val dx: Double = ep.x - sp.x
      val dy = ep.y - sp.y
      val n = norm(DenseVector[Double](dx, dy))

      val n2: Double = Math.sqrt((sp.x - ep.x) * (sp.x - ep.x) + (sp.y - ep.y) * (sp.y - ep.y))
      val rad = n / radius

      //if (Math.abs(dx) + Math.abs(dy) > 0.1) true else false
      if (
      // (n > 180.0 && radius < 99999.0 && radius2 < 99999.0)



        (radius + radius2) / 2 / bulge < minArcBugle
      //&&   rad>arcRatio && radius>arcRadiiusRange._1 && radius<arcRadiiusRange._2
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
      val oy = Math.round(offset.y) * 10.0d



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
          buf += MachineItem(Left(arc.originalEP))
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
      val isOuter: Boolean = if (contour.name.equals("CUTH")) false else true


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

      if ((Math.abs(dx) + Math.abs(dy)) / 2 > 10) {
        buff += Point(dx, dy).toESSI
        currPos = pOffset
      }


      buff += "6"
      buff += "7"
      buff += "29" //left compensation 30//rightcompensation


      contour.ops.foreach(op => {

        op.pointOrArc match {
          case Right(arc: Arc) => {
            //val r = arc.R
            val dir = if (isClockWise(arc)) "+" else "-"


            val dx = arc.sp.x - currPos.x
            val dy = arc.sp.y - currPos.y
            if ((Math.abs(dx) + Math.abs(dy)) / 2 > 10) {
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
            if ((Math.abs(dx) + Math.abs(dy)) / 2 > 10) {
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
      buff += "38"
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

  private def offsetArc(src: Arc, isOuter: Boolean): Arc = {
    val rotFactor = if (isOuter) 1.0 else -1.0

    //val rotFactor = if (isClockWise(this)) -1.0 else 1.0

    val correntSP: DenseVector[Double] = DenseVector[Double]((src.sp.x), (src.sp.y))
    val correntEp: DenseVector[Double] = DenseVector[Double]((src.ep.x), (src.ep.y))
    val correntSPVec: DenseVector[Double] = DenseVector[Double]((src.rotCenter.x - src.sp.x), (src.rotCenter.y - src.sp.y))
    val correntEPVec: DenseVector[Double] = DenseVector[Double]((src.rotCenter.x - src.ep.x), (src.rotCenter.y - src.ep.y))
    val newSP = {
      val BP_PLen: Double = norm(correntSPVec)
      val BP_PDir: DenseVector[Double] = (correntSPVec / BP_PLen) * rotFactor
      val offsetted: DenseVector[Double] = correntSP + (offset * BP_PDir)
      Point(offsetted(0), offsetted(1))
    }
    val newEP = {
      val BP_PLen: Double = norm(correntEPVec)
      val BP_PDir: DenseVector[Double] = (correntEPVec / BP_PLen) * rotFactor
      val offsetted: DenseVector[Double] = correntEp + (offset * BP_PDir)
      Point(offsetted(0), offsetted(1))
    }
    val retVal = Arc(newSP, src.rotCenter, newEP)
    retVal
  }

  private def offsetPoint(src: Point, beforeP: Point, nextP: Point, isOuter: Boolean): String = {

    val rotFactor = if (isOuter) 1.0 else -1.0

    val cp = DenseVector[Double](src.x, src.y)
    val bp = DenseVector[Double](beforeP.x, beforeP.y)
    val np = DenseVector[Double](nextP.x, nextP.y)


    /*    (x, y) rotated 90 degrees around(0, 0) is (-y, x). If you want to rotate clockwise , you simply do it the other way around, getting(y, -x)*/
    val BPtP: DenseVector[Double] = DenseVector[Double](cp(0) - bp(0), cp(1) - bp(1))
    val BPtPLen: Double = norm(BPtP)
    val BPtPDir: DenseVector[Double] = BPtP / BPtPLen
    val BPtPOffsetDir: DenseVector[Double] = DenseVector[Double](BPtPDir(1) * -1, BPtPDir(0))
    val bpcp: DenseVector[Double] = cp + (offset * BPtPOffsetDir)
    val BPCPVec: DenseVector[Double] = DenseVector[Double](cp(0) - bpcp(0), cp(1) - bpcp(1))

    val PtNP: DenseVector[Double] = DenseVector[Double](np(0) - cp(0), np(1) - cp(1))
    val PtNPLen: Double = norm(PtNP)
    val PtNPDir: DenseVector[Double] = PtNP / PtNPLen
    val PtNPOffsetDir: DenseVector[Double] = DenseVector[Double](PtNPDir(1) * -1, PtNPDir(0))
    val cpnp: DenseVector[Double] = cp + (offset * PtNPOffsetDir)
    //val CPNPVec: DenseVector[Double] = DenseVector[Double](cpnp(0) - cp(0), cpnp(1) - cp(1))

    val mp: DenseVector[Double] = DenseVector[Double]((bpcp(0) + cpnp(0)) / 2.0, (bpcp(1) + cpnp(1)) / 2.0)
    val PtMP: DenseVector[Double] = DenseVector[Double](mp(0) - cp(0), mp(1) - cp(1))

    val cosPHI: Double = ((Math.max(Math.min((BPCPVec dot PtMP) / (norm(BPCPVec) * norm(PtMP)), 1), -1)))
    val acosPHI: Double = Math.acos(cosPHI)
    val alsoSin = Math.sin(acosPHI)
    val sinPHI = Math.sqrt(1.0 - cosPHI * cosPHI)


    val tgPHI = sinPHI / cosPHI
    val extraLen = offset * tgPHI
    println(cosPHI + " " + sinPHI + " alsoSin " + alsoSin + " " + extraLen)

    val ofsetted: DenseVector[Double] = bpcp + (extraLen * BPtPDir)

    val retVal: Point = Point(ofsetted(0), ofsetted(1))

    retVal.toESSI
  }

}
