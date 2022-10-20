package local.hull.cnc.hyprethermGcode

import breeze.linalg.{DenseVector, norm}
import local.hull.cnc.hyprethermGcode.Cases._

import java.io.{File, PrintWriter}
import scala.collection.mutable.ListBuffer

object CNCManager extends ForanFileUtil {


  def doCNCStringsOld(in: List[String], username: String): List[String] = {
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
        case "CUT" => {
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

  def doCNCStringsOld2(in: List[String], username: String): List[String] = {
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
          op.ops.tail.foreach(c => {
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
          op.ops.tail.foreach(c => {
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
        case "CUT" => {
          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            buff += (cutToolOp)
          }
          val m1 = (toGcode(move, MachineItem(Left(getStartPos(op.ops.head))), offsetCorrection))
          if (!buff.last.equals(m1)) buff += m1
          lastOpPoint = getStartPos(op.ops.head)
          if (!buff.last.equals(startCutOuter)) buff += startCutOuter
          op.ops.tail.foreach(c => {
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

    //val pw = new PrintWriter(new File(pathOut))

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

  def doCNCStrings(in: List[String], username: String): List[String] = {
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
          op.ops.tail.foreach(c => {
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

          val freeMovePoint: Point = {
            op.ops.head.pointOrArc match {
              case Right(value: Arc) => value.sp
              case Left(value: Point) => value
            }
          }
          val freeMovePointEP: Point = {
            op.ops.head.pointOrArc match {
              case Right(value: Arc) => value.ep
              case Left(value: Point) => {
                op.ops(1).pointOrArc match {
                  case Right(value: Arc) => value.sp
                  case Left(value: Point) => value
                }
              }
            }
          }
          val pOffset: Point = cutOffseCalc(freeMovePoint, freeMovePointEP)

          val m1 = (toGcode(move, MachineItem(Left(pOffset)), offsetCorrection))
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
        case "CUT" => {
          if (!currentTool.equals(cutToolOp)) {
            currentTool = cutToolOp
            buff += (cutToolOp)
          }
          val m1 = (toGcode(move, MachineItem(Left(getStartPos(op.ops.head))), offsetCorrection))
          if (!buff.last.equals(m1)) buff += m1
          lastOpPoint = getStartPos(op.ops.head)
          if (!buff.last.equals(startCutOuter)) buff += startCutOuter
          op.ops.tail.foreach(c => {
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

  private def cutOffseCalc(sp: Point, ep: Point, isOuter: Boolean = false): Point = {
    val spX: Double = sp.x
    val spY: Double = sp.y
    val epX: Double = ep.x
    val epY: Double = ep.y
    val dist = -10.0
    val vX = epX - spX
    val vY = epY - spY
    val v: DenseVector[Double] = DenseVector[Double](vX, vY)
    val vNorm: Double = norm(v)
    val angle = Math.PI / 2.0
    val xRotated = spX + dist * ((vX * Math.cos(angle) - vY * Math.sin(angle)) / vNorm)
    val yRotated = spY + dist * ((vX * Math.sin(angle) + vY * Math.cos(angle)) / vNorm)
    Point(xRotated, yRotated)
  }
}
