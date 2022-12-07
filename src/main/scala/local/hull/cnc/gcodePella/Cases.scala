package local.hull.cnc.gcodePella

object Cases {

  val arcAlgoRadius: Boolean = false

  val commands: List[String] = List[String]("JUMP", "MARK", "CUTH", "CUT", "MKBN")
  def doubleToStr(in:Double):String={
    val ret= f"$in%.2f"

    if(ret.equals("-0.00"))"0.00" else ret
  }

  case class CNCcoordsPackage(num: Int, coords: List[Point])

  case class CNC(name: String, A: Int = 0, B: Int = 0, machineItems: List[CNCcoordsPackage])

  case class Point(x: Double, y: Double) {
    override def toString: String = s"${doubleToStr(x)} ${doubleToStr(y)}"

    def toGcode(cmd: String, offsetCorrection:Point): String = s"${cmd}X${doubleToStr(x-offsetCorrection.x)}Y${doubleToStr(y-offsetCorrection.y)}"
  }

  case class Arc(sp: Point, rotCenter: Point, ep: Point) {
    override def toString: String = s"sp=${sp.toString} rp=${rotCenter.toString} ep=${ep.toString}"

    def toGcode(cmd:String, offsetCorrection:Point):String={
      if(arcAlgoRadius){
        val r=Math.sqrt((rotCenter.x-sp.x)*(rotCenter.x-sp.x)+(rotCenter.y-sp.y)*(rotCenter.y-sp.y))
        s"${cmd}X${doubleToStr(ep.x)}Y${doubleToStr(ep.y)}R${r}"
      }else{
        val valueX=rotCenter.x-sp.x
        val valueY=rotCenter.y-sp.y
        val I: Double = {
          if(Math.abs(0.0-valueX)<0.01  && Math.abs(0.0-valueY)<0.01) 0.01 else valueX
        }
        val J: Double = {
          if(Math.abs(0.0-valueX)<0.01  && Math.abs(0.0-valueY)<0.01) 0.01 else valueY
        }
        s"${cmd}X${doubleToStr(ep.x-offsetCorrection.x)}Y${doubleToStr(ep.y-offsetCorrection.y)}I${doubleToStr(I)}J${doubleToStr(J)}"
      }
    }
  }

  case class MachineItem(pointOrArc: Either[Point, Arc]) {
/*    override def toString: String = pointOrArc match {
      case Right(value: Arc) => value.toString
      case Left(value: Point) => value.toString
    }*/
  }

  case class PseudoMachineOps(name: String, ops: List[MachineItem]) {
    override def toString: String = {
      var str: String = s"${name}\n"
      ops.foreach(op => {
        str += s"${op.toString}\n"
      })
      str
    }
  }

  val initOp: Point = Point(0, 0)

  val startOps: String ="G21\nG90\nG99 X1 Y0 I0 J0\nG40"// "G21\nG90\nG99X1Y0I0J0"

  val markToolOp: String ="M06"// "M11"

  //val cutToolOp: String ="M00\nM06" //"M12"  G40\nG00X0.00 Y0.00\nG40
  val cutToolOp: String ="G00X0.00Y0.00\nG40\nM00\nM06"

  val finishOp: String = "M30"

  val markPauseOp: String = "G04 P0.5"

  val cutPauseOp: String = "G04 P1.8"

  val startMark: String = "G40\nM07"

  val stopMark: String = "M08\nG40"

  val startCutHoles: String = "G41\nM07"

  val startCutOuter: String = "G41\nM07"

  val stopCut: String = "M08\nG40"

  val move: String = "G00"

  val stright: String = "G01"

  val arcCW: String = "G03"

  val arcACW: String = "G02"

}
