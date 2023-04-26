package local.ele.cbfill.models

import org.bson.types.ObjectId

class CableBoxModule (val _id: ObjectId, val name: String, val shortName: String,
                      val material: String,val vendorcode: String, val trmCode: String,
                      val k: Int = 5,val columns:Int, val h: Double, val l: Double, val b: Double,
                      val H: Double, val L: Double, val B: Double, val weight: Double){


  val unitH: Int = (H / k).toInt
  val unitL: Int = (L / k).toInt
  val unitB: Int = (B / k).toInt

  val unitArea: Int = unitH * unitL
  val area: Double = H * L


}
