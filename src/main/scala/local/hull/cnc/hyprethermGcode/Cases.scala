package local.hull.cnc.hyprethermGcode

object Cases {

  val arcAlgoRadius: Boolean = false

  val commands: List[String] = List[String]("JUMP", "MARK", "CUTH", "CUT")
  def doubleToStr(in:Double):String=f"$in%.2f"

  case class CNCcoordsPackage(num: Int, coords: List[Point])

  case class CNC(name: String, A: Int = 0, B: Int = 0, machineItems: List[CNCcoordsPackage])

  case class Point(x: Double, y: Double) {
    override def toString: String = s"${doubleToStr(x)} ${doubleToStr(y)}"

    def toGcode(cmd: String): String = s"${cmd}X${doubleToStr(x)}Y${doubleToStr(y)}"
  }

  case class Arc(sp: Point, rotCenter: Point, ep: Point) {
    override def toString: String = s"sp=${sp.toString} rp=${rotCenter.toString} ep=${ep.toString}"

    def toGcode(cmd:String):String={
      if(arcAlgoRadius){
        val r=Math.sqrt((rotCenter.x-sp.x)*(rotCenter.x-sp.x)+(rotCenter.y-sp.y)*(rotCenter.y-sp.y))
        s"${cmd}X${doubleToStr(ep.x)}Y${doubleToStr(ep.y)}R${r}"
      }else{
        val I=rotCenter.x-sp.x
        val J=rotCenter.y-sp.y
        s"${cmd}X${doubleToStr(ep.x)}Y${doubleToStr(ep.y)}I${doubleToStr(I)}J${doubleToStr(J)}"
      }
    }
    /*   def toGcode(cmd:String):String={
          val r=Math.sqrt((rotCenter.x-sp.x)*(rotCenter.x-sp.x)+(rotCenter.y-sp.y)*(rotCenter.y-sp.y))
          s"${cmd}X${ep.x.toString}Y${ep.y.toString}R${r}"
        }*/

    /*    def toGcode(cmd: String): String ={
          val r=Math.sqrt((rotCenter.x-sp.x)*(rotCenter.x-sp.x)+(rotCenter.y-sp.y)*(rotCenter.y-sp.y))
          val I=rotCenter.x-sp.x
          val J=rotCenter.y-sp.y

          s"${cmd}X${ep.x.toString}Y${ep.y.toString}I${I.toString}J${J.toString}"
        }*/
  }

  case class MachineItem(pointOrArc: Either[Point, Arc]) {
    override def toString: String = pointOrArc match {
      case Right(value: Arc) => value.toString
      case Left(value: Point) => value.toString
    }
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

  val startOps: String = "G21\nG40\nG90"

  val markToolOp = "M37T5\nM11"

  val cutToolOp = "M37T3\nM12"

  val finishOp = "M02"

  val markPauseOp = "G04 P0.5"

  val cutPauseOp = "G04 P1.8"

  val startMark = "M09"

  val stopMark = "M10"

  val startCut = "M07\nG41"

  val stopCut = "M08"

  val move = "G00"

  val stright = "G01"

  val arcCW = "G03"

  val arcACW = "G02"

}
