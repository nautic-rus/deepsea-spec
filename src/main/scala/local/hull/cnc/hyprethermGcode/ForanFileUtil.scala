package local.hull.cnc.hyprethermGcode

import local.hull.cnc.hyprethermGcode.Cases._

import java.io.{File, PrintWriter}
import scala.Double.NaN
import scala.collection.mutable.ListBuffer
import scala.io.Source

trait ForanFileUtil {

  def procForanCLPfromFile(path: String): List[CNC] = {
    val src = Source.fromFile(path)
    val lines = src.getLines.toList
    src.close()
    procForanCLPfromList(lines)
  }

  def procForanCLPfromList(lines: List[String]): List[CNC] = {
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
    concattedCommands.foreach(c => buff += doCNC(c))
    buff.toList
  }

  def genPTfile(path: String, cnc: List[CNC]): Unit = {
    val pw = new PrintWriter(new File(path))
    cnc.foreach(c => {
      c.machineItems.foreach(item => {
        item.coords.foreach(p => {
          pw.println(p.x + " " + p.y + " 0.0 ")
        })
      })
    })
    pw.close()
  }

  private def doCommand(in: List[String]): String = {
    var str = ""
    in.foreach(l => str += l)
    str.replace("  ", " ").replace("   ", " ").trim
  }

  private def doCNC(in: String): CNC = {
    val arr = in.split(" ")
    arr.head match {
      case "JUMP" => {
        CNC(arr.head, 1, 0, List[CNCcoordsPackage](CNCcoordsPackage(1, doubleListToPoints(List[Double](arr(1).toDoubleOption.getOrElse(0.0), arr(2).toDoubleOption.getOrElse(0.0))))))
      }
      case "MARK" => {
        val lb = ListBuffer.empty[CNCcoordsPackage]
        val A = arr(1).toIntOption.getOrElse(0)
        val B = arr(2).toIntOption.getOrElse(0)
        val splitTogroup = in.split("999999 999999")
        val coords: List[Double] = {
          val buff = ListBuffer.empty[Double]
          splitTogroup.head.split(" ").drop(3).foreach(d => buff += d.toDoubleOption.getOrElse(0.0))
          buff.toList
        }
        lb += CNCcoordsPackage(1, doubleListToPoints(coords))
        var counter = 2
        splitTogroup.tail.foreach(gr => {
          val buff = ListBuffer.empty[Double]
          gr.trim.split(" ").foreach(d => buff += d.toDoubleOption.getOrElse(0.0))
          lb += CNCcoordsPackage(counter, doubleListToPoints(buff.toList))
          counter = counter + 1
        })
        CNC(arr.head, A, B, lb.toList)
      }
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

        lb += CNCcoordsPackage(1, doubleListToPoints(coords))
        var counter = 2
        splitTogroup.tail.foreach(gr => {
          val buff = ListBuffer.empty[Double]
          gr.trim.split(" ").foreach(d => buff += d.toDoubleOption.getOrElse(0.0))
          lb += CNCcoordsPackage(counter, doubleListToPoints(buff.toList))
          counter = counter + 1
        })
        CNC(arr.head, A, B, lb.toList)
      }
      case _ => CNC("ERROR " + arr.head, 0, 0, List[CNCcoordsPackage](CNCcoordsPackage(1, doubleListToPoints(List[Double]()))))
    }
  }

  def isClockWise(arc: Arc): Boolean = {
    val xRot: Double = arc.rotCenter.x
    val yRot: Double = arc.rotCenter.y
    val xStrart: Double = arc.sp.x
    val yStart: Double = arc.sp.y
    val xEnd: Double = arc.ep.x
    val yEnd: Double = arc.ep.y
    (xStrart - xRot) * (yEnd - yRot) - (yStart - yRot) * (xEnd - xRot) > 0
  }

  private def doubleListToPoints(in: List[Double]): List[Point] = {
    val buffer = ListBuffer.empty[Point]
    (0 until in.length - 1 by 2).foreach(i => {
      buffer += Point(in(i), in(i + 1))
    })
    buffer.toList
  }

  def isPoinValid(p: Point): Boolean = if (p.x != NaN && p.y != NaN) true else false

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

  def genPseudoMachineOps(in: List[CNC]): List[PseudoMachineOps] = {
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

  def getLastPos(in: MachineItem): Point = {
    in.pointOrArc match {
      case Right(value) => value.ep
      case Left(value) => value
    }
  }

  def getStartPos(in: MachineItem): Point = {
    in.pointOrArc match {
      case Right(value) => value.sp
      case Left(value) => value
    }
  }

  def toGcode(cmd: String, in: MachineItem): String = {
    in.pointOrArc match {
      case Right(value) => value.toGcode(cmd)
      case Left(value) => value.toGcode(cmd)
    }
  }

  def pointsEquals(p1: Point, p2: Point): Boolean = {
    if (Math.sqrt((p1.x - p2.x) * (p1.x - p2.x) + (p1.y - p2.y) * (p1.y - p2.y)) < 0.01d) true else false
  }

  def checkDigitOrPlusMinus(in: String): Boolean = {
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

}
