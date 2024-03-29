package local.hull.cnc.gcodePella

import local.hull.cnc.gcodePella.Cases._

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

object CNCManager extends ForanFileUtil {


  def doCNCStrings(input: List[String], username: String): List[String] = {
    val in = fixCUTL(input)
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
    val buff = ListBuffer.empty[String]
    val clp: List[CNC] = procForanCLPfromList(in)
    val machineOpts: List[PseudoMachineOps] = genPseudoMachineOps(clp)

    buff += ("(This file has been generated for use ONLY on CelikTrans's HYPERTHERM plasma table)")
    if (username.nonEmpty) buff += ("(designed by " + username + ")")
    in.find(p => p.startsWith("PLAT")) match {
      case Some(value) => {
        buff += ("(" + value.replace("\n", "") + ")")
      }
      case None => None
    }
    buff += (startOps)
    var currentTool = ""
    var lastOpPoint: Point = initOp

    machineOpts.foreach(op => {
      op.name match {
        case "JUMP" => None

        case "MARK" | "MKBN" => {
          if (!currentTool.equals(markToolOp)) {
            currentTool = markToolOp
            buff += (markToolOp)
          }
          val m1 = (toGcode(move, MachineItem(Left(getStartPos(op.ops.head))), offsetCorrection))
          if (!buff.last.equals(m1)) buff += m1
          lastOpPoint = getStartPos(op.ops.head)
          if (!buff.last.equals(startMark)) buff += startMark
          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value.sp)), offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                }
                if (isClockWise(value)) {
                  val m1 = (toGcode(arcCW, c, offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                  lastOpPoint = getLastPos(c)
                } else {
                  val m1 = (toGcode(arcACW, c, offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value)), offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                }
                val m1 = (toGcode(stright, c, offsetCorrection))
                if (!buff.last.equals(m1)) buff += m1
                lastOpPoint = getLastPos(c)
              }
            }
          })
          if (!buff.last.equals(stopMark)) buff += stopMark
        }
        case "CUTH" => {
          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            buff += (cutToolOp)
          }
          val m1 = (toGcode(move, MachineItem(Left(getStartPos(op.ops.head))), offsetCorrection))
          if (!buff.last.equals(m1)) buff += m1
          lastOpPoint = getStartPos(op.ops.head)
          if (!buff.last.equals(startCutHoles)) buff += startCutHoles
          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value.sp)), offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                }
                if (isClockWise(value)) {
                  val m1 = (toGcode(arcCW, c, offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                  lastOpPoint = getLastPos(c)
                } else {
                  val m1 = (toGcode(arcACW, c, offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value)), offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                }
                val m1 = (toGcode(stright, c, offsetCorrection))
                if (!buff.last.equals(m1)) buff += m1
                lastOpPoint = getLastPos(c)
              }
            }
          })
          if (!buff.last.equals(stopCut)) buff += stopCut
        }
        case "CUT" | "CUTX" => {
          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            buff += (cutToolOp)
          }
          val m1 = (toGcode(move, MachineItem(Left(getStartPos(op.ops.head))), offsetCorrection))
          if (!buff.last.equals(m1)) buff += m1
          lastOpPoint = getStartPos(op.ops.head)
          if (!buff.last.equals(startCutOuter)) buff += startCutOuter
          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value.sp)), offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                }
                if (isClockWise(value)) {
                  val m1 = (toGcode(arcCW, c, offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                  lastOpPoint = getLastPos(c)
                } else {
                  val m1 = (toGcode(arcACW, c, offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value)), offsetCorrection))
                  if (!buff.last.equals(m1)) buff += m1
                }
                val m1 = (toGcode(stright, c, offsetCorrection))
                if (!buff.last.equals(m1)) buff += m1
                lastOpPoint = getLastPos(c)
              }
            }
          })
          if (!buff.last.equals(stopCut)) buff += stopCut
        }
        case _ => None
      }
    })

    buff += (finishOp)
    buff.toList
  }


  def doCNC(in: List[String], pathOut: String, username: String): String = {

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


    val clp: List[CNC] = procForanCLPfromList(in)
    val machineOpts: List[PseudoMachineOps] = genPseudoMachineOps(clp)

    val retStrBuff = ListBuffer.empty[String]


    retStrBuff += ("(This file has been generated for use ONLY on CelikTrans's HYPERTHERM plasma table)")
    if (username.nonEmpty) retStrBuff += "(designed by " + username + ")"
    in.find(p => p.startsWith("PLAT")) match {
      case Some(value) => {
        retStrBuff += ("(" + value.replace("\n", "") + ")")
      }
      case None => None
    }
    retStrBuff += (startOps)
    var currentTool = ""
    var lastOpPoint: Point = initOp

    machineOpts.foreach(op => {

      op.name match {

        case "JUMP" => {
          // pw.println(toGcode(move,op.ops.head))
          //lastOpPoint=getLastPos(op.ops.head)
          //pw.println(toGcode(move,MachineItem(Left(getStartPos(op.ops.head)))))
        }
        case "MARK" | "MKBN" => {
          if (!currentTool.equals(markToolOp)) {
            currentTool = markToolOp
            if (!retStrBuff.last.equals(markToolOp)) retStrBuff += (markToolOp)
          }
          val m1 = (toGcode(move, MachineItem(Left(getStartPos(op.ops.head))), offsetCorrection))
          if (!retStrBuff.last.equals(m1)) retStrBuff += m1
          lastOpPoint = getStartPos(op.ops.head)
          if (!retStrBuff.last.equals(startMark)) retStrBuff += startMark
          //if(!retStrBuff.last.equals(markPauseOp)) retStrBuff+=(markPauseOp)
          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {

                if (!pointsEquals(value.sp, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value.sp)), offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                }


                if (isClockWise(value)) {
                  val m1 = (toGcode(arcCW, c, offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1

                  lastOpPoint = getLastPos(c)
                } else {
                  val m1 = (toGcode(arcACW, c, offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value)), offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                }

                val m1 = (toGcode(stright, c, offsetCorrection))
                if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                lastOpPoint = getLastPos(c)
              }
            }
          })

          if (!retStrBuff.last.equals(stopMark)) retStrBuff += stopMark
        }

        case "CUTH" => { //
          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            if (!retStrBuff.last.equals(cutToolOp)) retStrBuff += cutToolOp
          }
          val m1 = (toGcode(move, MachineItem(Left(getStartPos(op.ops.head))), offsetCorrection))
          if (!retStrBuff.last.equals(m1)) retStrBuff += m1
          lastOpPoint = getStartPos(op.ops.head)
          if (!retStrBuff.last.equals(startCutHoles)) retStrBuff += startCutHoles
          //if(!retStrBuff.last.equals(cutPauseOp))retStrBuff+=cutPauseOp


          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value.sp)), offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                }

                if (isClockWise(value)) {
                  val m1 = (toGcode(arcCW, c, offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                  lastOpPoint = getLastPos(c)
                } else {
                  val m1 = (toGcode(arcACW, c, offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value)), offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                }
                val m1 = (toGcode(stright, c, offsetCorrection))
                if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                lastOpPoint = getLastPos(c)
              }
            }
          })
          if (!retStrBuff.last.equals(stopCut)) retStrBuff += stopCut
        }

        case "CUT" => { //
          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            if (!retStrBuff.last.equals(cutToolOp)) retStrBuff += cutToolOp
          }
          val m1 = (toGcode(move, MachineItem(Left(getStartPos(op.ops.head))), offsetCorrection))
          if (!retStrBuff.last.equals(m1)) retStrBuff += m1
          lastOpPoint = getStartPos(op.ops.head)
          if (!retStrBuff.last.equals(startCutOuter)) retStrBuff += startCutOuter
          //if(!retStrBuff.last.equals(cutPauseOp))retStrBuff+=cutPauseOp
          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value.sp)), offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                }
                if (isClockWise(value)) {
                  val m1 = (toGcode(arcCW, c, offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                  lastOpPoint = getLastPos(c)
                } else {
                  val m1 = (toGcode(arcACW, c, offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) {
                  val m1 = (toGcode(stright, MachineItem(Left(value)), offsetCorrection))
                  if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                }
                val m1 = (toGcode(stright, c, offsetCorrection))
                if (!retStrBuff.last.equals(m1)) retStrBuff += m1
                lastOpPoint = getLastPos(c)
              }
            }
          })
          if (!retStrBuff.last.equals(stopCut)) retStrBuff += stopCut
        }

        case _ => None
      }

    })


    if (!retStrBuff.last.equals(finishOp)) retStrBuff += finishOp


    val pw = new PrintWriter(new File(pathOut))
    retStrBuff.foreach(line => pw.println(line))

    pw.close()
    pathOut
  }

  private def fixCUTL(in: List[String]): List[String] = {
    val out = ListBuffer.empty[String]
    var cutl = ""
    var jmp = ""
    in.foreach(str => {
      if (str.contains("JUMP")) {
        jmp = str.replace("JUMP ", "")
      }
      if (str.contains("CUTL")) {
        cutl = str.replace("CUTL ", "")
      }
      if (str.contains("CUT ") && cutl.nonEmpty) {
        val groups = str.split("  ")
        val nstr = groups(0) + "  " + groups(1) + "  " + jmp + " 999999 999999 " + groups(2)
        out += nstr
        cutl = ""
        jmp = ""
      } else {
        out += str
      }
    })
    out.toList
  }

}
