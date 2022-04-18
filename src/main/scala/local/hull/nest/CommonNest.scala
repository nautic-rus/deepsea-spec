package local.hull.nest

import local.hull.bill.BillManager.ForanScrap

object CommonNest {

  case class NestIdBlock(nestID: String, block: String)

  case class NestMaterial(MATERIAL: String = "",
                          THICKNESS: Double = 0.0,
                          NEST_LENGTH: Int = 0,
                          NEST_WIDTH: Int = 0,
                          PARENTID: String = "",
                          GROSSWEIGHT:Int=0
                         )

  case class Nest(
                   ID: String = "",
                   MATERIAL: String = "",
                   THICKNESS: Double = 0.0,
                   NEST_LENGTH: Int = 0,
                   NEST_WIDTH: Int = 0,
                   NUM_EQ_NEST: Int = 0,
                   PARTS_WEIGHT: Double = 0.0,
                   DENSITY: Double = 0.0,
                   USAGE: Double = 0.0,
                   BLOCKS: String = "",
                   isLock: Boolean = false,
                   lockInfo: NestLock = NestLock(),
                   PARENTID: String = "",
                   GROSSWEIGHT:Int=0,
                   SCRAP:ForanScrap=ForanScrap()
                 )

  case class NestLock(project: String = "", nestId: String = "", date: Long = 0, user: String = "")


  def calculateWeightKG(Wmm:Double,Lmm:Double, Tmm:Double,ROgrammBySm3:Double):Int=Math.ceil(((Wmm/10.0*Lmm/10.0*Tmm/10.0)*ROgrammBySm3)/1000.0).toInt

}
