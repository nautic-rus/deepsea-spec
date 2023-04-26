package local.ele.cbfill.models

import org.bson.types.ObjectId

class SealModule(val _id: ObjectId, val name: String, val vendorcode: String, val trmCode: String,
                 val k: Int = 5, val H: Double, val L: Double, val B: Double,
                 val cabDMin: Double, val cabDMax: Double, val weight: Double) {


  val unitH: Int = (H / k).toInt
  val unitL: Int = (L / k).toInt
  val unitB: Int = (B / k).toInt

  val unitArea: Int = unitH * unitL
  val area: Double = H * L

  val isDummy: Boolean = cabDMax - cabDMin <= 0.0

  def isFitCable(cableDiam: Double): Boolean = cabDMin <= cableDiam && cableDiam <= cabDMax

  override def toString: String = {
    unitL+"x"+unitH
  }

}

