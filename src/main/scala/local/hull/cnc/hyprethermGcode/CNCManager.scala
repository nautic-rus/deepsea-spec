package local.hull.cnc.hyprethermGcode

import local.hull.cnc.hyprethermGcode.Cases._

import java.io.{File, PrintWriter}

object CNCManager extends ForanFileUtil {


  def doCNC(in: List[String], pathOut: String, username:String): String = {

    val clp: List[CNC] = procForanCLPfromList(in)
    val hasMark: Boolean = clp.exists(p => p.name.equals("MARK"))

    val machineOpts: List[PseudoMachineOps] = genPseudoMachineOps(clp)
    val pw = new PrintWriter(new File(pathOut))
    pw.println(startOps(hasMark))
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
