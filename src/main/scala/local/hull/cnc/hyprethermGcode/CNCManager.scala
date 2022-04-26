package local.hull.cnc.hyprethermGcode

import local.hull.cnc.hyprethermGcode.Cases._

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

object CNCManager extends ForanFileUtil {


  def doCNCStrings(in: List[String], username: String): List[String]={
    val offsetCorrection: Point = in.find(s => s.startsWith("STAR")) match {
      case Some(value) =>{
        val arr=value.split(" ")
        if(arr.length==3 && checkDigitOrPlusMinus(arr(1)) &&  checkDigitOrPlusMinus(arr(2))){
          Point(arr(1).toDoubleOption.getOrElse(0.0d),arr(2).toDoubleOption.getOrElse(0.0d))
        }else{
          Point(0,0)
        }
      }
      case None => Point(0,0)
    }


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
          buff+=(toGcode(move, MachineItem(Left(getStartPos(op.ops.head))),offsetCorrection))
          lastOpPoint = getStartPos(op.ops.head)
          buff+=(startMark)
          buff+=(markPauseOp)
          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) buff+=(toGcode(stright, MachineItem(Left(value.sp)),offsetCorrection))
                if (isClockWise(value)) {
                  buff+=(toGcode(arcCW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                } else {
                  buff+=(toGcode(arcACW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) buff+=(toGcode(stright, MachineItem(Left(value)),offsetCorrection))
                buff+=(toGcode(stright, c,offsetCorrection))
                lastOpPoint = getLastPos(c)
              }
            }
          })
          buff+=(stopMark)

        }
        case "CUTH" => { //

          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            buff+=(cutToolOp)
          }
          buff+=(toGcode(move, MachineItem(Left(getStartPos(op.ops.head))),offsetCorrection))
          lastOpPoint = getStartPos(op.ops.head)
          buff+=(startCutHoles)
          buff+=(cutPauseOp)

          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) buff+=(toGcode(stright, MachineItem(Left(value.sp)),offsetCorrection))
                if (isClockWise(value)) {
                  buff+=(toGcode(arcCW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                } else {
                  buff+=(toGcode(arcACW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) buff+=(toGcode(stright, MachineItem(Left(value)),offsetCorrection))
                buff+=(toGcode(stright, c,offsetCorrection))
                lastOpPoint = getLastPos(c)
              }
            }
          })
          buff+=(stopCut)
        }

        case "CUT" => {
          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            buff+=(cutToolOp)
          }
          buff+=(toGcode(move, MachineItem(Left(getStartPos(op.ops.head))),offsetCorrection))
          lastOpPoint = getStartPos(op.ops.head)
          buff+=(startCutOuter)
          buff+=(cutPauseOp)

          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) buff+=(toGcode(stright, MachineItem(Left(value.sp)),offsetCorrection))
                if (isClockWise(value)) {
                  buff+=(toGcode(arcCW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                } else {
                  buff+=(toGcode(arcACW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) buff+=(toGcode(stright, MachineItem(Left(value)),offsetCorrection))
                buff+=(toGcode(stright, c,offsetCorrection))
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

    val offsetCorrection: Point = in.find(s => s.startsWith("STAR")) match {
      case Some(value) =>{
        val arr=value.split(" ")
        if(arr.length==3 && checkDigitOrPlusMinus(arr(1)) &&  checkDigitOrPlusMinus(arr(2))){
          Point(arr(1).toDoubleOption.getOrElse(0.0d),arr(2).toDoubleOption.getOrElse(0.0d))
        }else{
          Point(0,0)
        }
      }
      case None => Point(0,0)
    }


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
          pw.println(toGcode(move, MachineItem(Left(getStartPos(op.ops.head))),offsetCorrection))
          lastOpPoint = getStartPos(op.ops.head)
          pw.println(startMark)
          pw.println(markPauseOp)
          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) pw.println(toGcode(stright, MachineItem(Left(value.sp)),offsetCorrection))
                if (isClockWise(value)) {
                  pw.println(toGcode(arcCW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                } else {
                  pw.println(toGcode(arcACW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) pw.println(toGcode(stright, MachineItem(Left(value)),offsetCorrection))
                pw.println(toGcode(stright, c,offsetCorrection))
                lastOpPoint = getLastPos(c)
              }
            }
          })
          pw.println(stopMark)

        }

        case "CUTH"  => { //
          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            pw.println(cutToolOp)
          }
          pw.println(toGcode(move, MachineItem(Left(getStartPos(op.ops.head))),offsetCorrection))
          lastOpPoint = getStartPos(op.ops.head)
          pw.println(startCutHoles)
          pw.println(cutPauseOp)

          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) pw.println(toGcode(stright, MachineItem(Left(value.sp)),offsetCorrection))
                if (isClockWise(value)) {
                  pw.println(toGcode(arcCW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                } else {
                  pw.println(toGcode(arcACW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) pw.println(toGcode(stright, MachineItem(Left(value)),offsetCorrection))
                pw.println(toGcode(stright, c,offsetCorrection))
                lastOpPoint = getLastPos(c)
              }
            }
          })
          pw.println(stopCut)
        }

        case "CUT" => { //
          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            pw.println(cutToolOp)
          }
          pw.println(toGcode(move, MachineItem(Left(getStartPos(op.ops.head))),offsetCorrection))
          lastOpPoint = getStartPos(op.ops.head)
          pw.println(startCutOuter)
          pw.println(cutPauseOp)
          op.ops.foreach(c => {
            c.pointOrArc match {
              case Right(value: Arc) => {
                if (!pointsEquals(value.sp, lastOpPoint)) pw.println(toGcode(stright, MachineItem(Left(value.sp)),offsetCorrection))
                if (isClockWise(value)) {
                  pw.println(toGcode(arcCW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                } else {
                  pw.println(toGcode(arcACW, c,offsetCorrection))
                  lastOpPoint = getLastPos(c)
                }
              }
              case Left(value: Point) => {
                if (!pointsEquals(value, lastOpPoint)) pw.println(toGcode(stright, MachineItem(Left(value)),offsetCorrection))
                pw.println(toGcode(stright, c,offsetCorrection))
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
