package local.ele.cbfill.models

import org.bson.types.ObjectId

class CompressionBlock(val _id: ObjectId, val name: String, val vendorcode: String, val trmCode: String,
                       val k: Int = 5, val H: Double, val L: Double, val B: Double, val weight: Double) {

  val unitH: Int = (H / k).toInt
  val unitL: Int = (L / k).toInt
  val unitB: Int = (B / k).toInt


}

