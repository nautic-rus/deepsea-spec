package local.hull.cnc.hyprethermESSI

import scala.Double.NaN
import scala.collection.mutable.ListBuffer

object EssiCNCManagerOld {
  private val arcAlgoRadius: Boolean = false

  private val commands: List[String] = List[String]("CUTH", "CUT")

  private case class Point(x: Double, y: Double, offsetX: Double = 0.0, offsetY: Double = 0.0) {
    override def toString: String = s"${doubleToStr(x - offsetX)} ${doubleToStr(y - offsetY)}"

    def toESSI: String = {
      var ret = ""
      val xv = x - offsetX
      val yv = y - offsetY

      val strx = (Math.abs(xv) * 10).toInt.toString
      val stry = (Math.abs(yv) * 10).toInt.toString

      if (xv < 0) ret += "-" + strx else ret += "+" + strx
      if (yv < 0) ret += "-" + stry else ret += "+" + stry

      ret
    }

    //def toGcode(cmd: String, offsetCorrection:Point): String = s"${cmd}X${doubleToStr(x-offsetCorrection.x)}Y${doubleToStr(y-offsetCorrection.y)}"
  }

  private case class CNCcoordsPackage(num: Int, coords: List[Point])

  private case class CNC(name: String, A: Int = 0, B: Int = 0, machineItems: List[CNCcoordsPackage])

  private case class Arc(sp: Point, rotCenter: Point, ep: Point) {
    override def toString: String = s"sp=${sp.toString} rp=${rotCenter.toString} ep=${ep.toString}"

    def toGcode(cmd: String): String = {
      if (arcAlgoRadius) {
        val r = Math.sqrt((rotCenter.x - sp.x) * (rotCenter.x - sp.x) + (rotCenter.y - sp.y) * (rotCenter.y - sp.y))
        s"${cmd}X${doubleToStr(ep.x)}Y${doubleToStr(ep.y)}R${r}"
      } else {
        val valueX = rotCenter.x - sp.x
        val valueY = rotCenter.y - sp.y
        val I: Double = {
          if (Math.abs(0.0 - valueX) < 0.01 && Math.abs(0.0 - valueY) < 0.01) 0.01 else valueX
        }
        val J: Double = {
          if (Math.abs(0.0 - valueX) < 0.01 && Math.abs(0.0 - valueY) < 0.01) 0.01 else valueY
        }
        s"${cmd}X${doubleToStr(ep.x)}Y${doubleToStr(ep.y)}I${doubleToStr(I)}J${doubleToStr(J)}"
      }
    }

    def R: String = ((Math.sqrt((rotCenter.x - sp.x) * (rotCenter.x - sp.x) + (rotCenter.y - sp.y) * (rotCenter.y - sp.y))) * 10).toInt.toString

  }

  private case class MachineItem(pointOrArc: Either[Point, Arc]) {
    /*    override def toString: String = pointOrArc match {
          case Right(value: Arc) => value.toString
          case Left(value: Point) => value.toString
        }*/
  }

  private case class PseudoMachineOps(name: String, ops: List[MachineItem]) {
    override def toString: String = {
      var str: String = s"${name}\n"
      ops.foreach(op => {
        str += s"${op.toString}\n"
      })
      str
    }
  }


  def doEssiCNC(in: List[String], pathOut: String, username: String="Misha"): List[String] = {

    val lineBuff=ListBuffer.empty[String]

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


    val plate: String =in.find(p => p.startsWith("PLAT")) match {
      case Some(value) => {
        val str= ( value.replace("\n", "+")).split(" ")
        if(str.length==3){
          //"+"+str(1)+"0+"+str(2)+"0"
          "+100+100"
        }else{
          "+100+100"
        }
      }
      case None =>  "+100+100"
    }
    lineBuff+=plate


    val clp: List[CNC] = procForanCLPfromList(in, offsetCorrection)
    val machineOpts: List[PseudoMachineOps] = genPseudoMachineOps(clp)
    // val ret = doContour(machineOpts.head)
    val ret: List[String] = doContours(machineOpts)
    lineBuff++=ret
    //val pw = new PrintWriter(new File(pathOut))
    //ret.foreach(line => pw.println(line))
    //pw.close()
    //val jj = 0

    lineBuff.toList
  }


  private def doubleToStr(in: Double): String = {
    val ret = ((in * 10.0).toInt).toString
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
      buffer += Point(in(i), in(i + 1), offset.x, offset.y)
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
      arcs.foreach(arc => buf += MachineItem(Right(arc)))
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
      buff += "29"//left compensation 30//rightcompensation
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
