package local.hull.cnc.hyprethermGcode

import local.hull.cnc.hyprethermGcode.Cases._

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

object CNCManager extends ForanFileUtil {


  def doCNCStrings(in: List[String], username: String): List[String]={
    val buff=ListBuffer.empty[String]

    val clp: List[CNC] = procForanCLPfromList(in)
    val machineOpts: List[PseudoMachineOps] = genPseudoMachineOps(clp)

    buff+=("(This file has been generated for use ONLY on CelikTrans's HYPERTHERM plasma table)")
    if (username.nonEmpty) buff+=("(designed by " + username + ")")
    in.find(p => p.startsWith("PLAT")) match {
      case Some(value) => {
        buff+=("(" + value.replace("\n", "") + ")")
      }
      case None => None
    }
    buff+=(startOps)
    var currentTool = ""
    var lastOpPoint: Point = initOp
    machineOpts.foreach(op => {

      op.name match {
        case "MARK" => {
          if (!currentTool.equals(markToolOp)) {
            currentTool = markToolOp
            buff+=(markToolOp)
          }
          buff+=(toGcode(move, MachineItem(Left(getStartPos(op.ops.head)))))
          lastOpPoint = getStartPos(op.ops.head)
          buff+=(startMark)
          buff+=(markPauseOp)
          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) buff+=(toGcode(stright, MachineItem(Left(value.sp))))
                if (isClockWise(value)) {
                  buff+=(toGcode(arcCW, c))
                  lastOpPoint = getLastPos(c)
                } else {
                  buff+=(toGcode(arcACW, c))
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) buff+=(toGcode(stright, MachineItem(Left(value))))
                buff+=(toGcode(stright, c))
                lastOpPoint = getLastPos(c)
              }
            }
          })
          buff+=(stopMark)

        }
        case "CUTH" | "CUT" => { //

          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            buff+=(cutToolOp)
          }
          buff+=(toGcode(move, MachineItem(Left(getStartPos(op.ops.head)))))
          lastOpPoint = getStartPos(op.ops.head)
          buff+=(startCut)
          buff+=(cutPauseOp)

          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) buff+=(toGcode(stright, MachineItem(Left(value.sp))))
                if (isClockWise(value)) {
                  buff+=(toGcode(arcCW, c))
                  lastOpPoint = getLastPos(c)
                } else {
                  buff+=(toGcode(arcACW, c))
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) buff+=(toGcode(stright, MachineItem(Left(value))))
                buff+=(toGcode(stright, c))
                lastOpPoint = getLastPos(c)
              }
            }
          })
          buff+=(stopCut)
        }
        case _ => None
      }

    })
    buff+=(finishOp)

    buff.toList
  }


  def doCNC(in: List[String], pathOut: String, username: String): String = {
    val clp: List[CNC] = procForanCLPfromList(in)
    val machineOpts: List[PseudoMachineOps] = genPseudoMachineOps(clp)
    val pw = new PrintWriter(new File(pathOut))
    pw.println("(This file has been generated for use ONLY on CelikTrans's HYPERTHERM plasma table)")
    if (username.nonEmpty) pw.println("(designed by " + username + ")")
    in.find(p => p.startsWith("PLAT")) match {
      case Some(value) => {
        pw.println("(" + value.replace("\n", "") + ")")
      }
      case None => None
    }
    pw.println(startOps)
    var currentTool = ""
    var lastOpPoint: Point = initOp
    machineOpts.foreach(op => {

      op.name match {

        case "JUMP" => {
          // pw.println(toGcode(move,op.ops.head))
          //lastOpPoint=getLastPos(op.ops.head)
          //pw.println(toGcode(move,MachineItem(Left(getStartPos(op.ops.head)))))
        }
        case "MARK" => {
          if (!currentTool.equals(markToolOp)) {
            currentTool = markToolOp
            pw.println(markToolOp)
          }
          pw.println(toGcode(move, MachineItem(Left(getStartPos(op.ops.head)))))
          lastOpPoint = getStartPos(op.ops.head)
          pw.println(startMark)
          pw.println(markPauseOp)
          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) pw.println(toGcode(stright, MachineItem(Left(value.sp))))
                if (isClockWise(value)) {
                  pw.println(toGcode(arcCW, c))
                  lastOpPoint = getLastPos(c)
                } else {
                  pw.println(toGcode(arcACW, c))
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) pw.println(toGcode(stright, MachineItem(Left(value))))
                pw.println(toGcode(stright, c))
                lastOpPoint = getLastPos(c)
              }
            }
          })
          pw.println(stopMark)

        }
        case "CUTH" | "CUT" => { //

          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            pw.println(cutToolOp)
          }
          pw.println(toGcode(move, MachineItem(Left(getStartPos(op.ops.head)))))
          lastOpPoint = getStartPos(op.ops.head)
          pw.println(startCut)
          pw.println(cutPauseOp)

          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) pw.println(toGcode(stright, MachineItem(Left(value.sp))))
                if (isClockWise(value)) {
                  pw.println(toGcode(arcCW, c))
                  lastOpPoint = getLastPos(c)
                } else {
                  pw.println(toGcode(arcACW, c))
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) pw.println(toGcode(stright, MachineItem(Left(value))))
                pw.println(toGcode(stright, c))
                lastOpPoint = getLastPos(c)
              }
            }
          })
          pw.println(stopCut)
        }
        case _ => None
      }

    })
    pw.println(finishOp)
    pw.close()
    pathOut
  }

}
