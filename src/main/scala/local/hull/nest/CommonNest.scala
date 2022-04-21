package local.hull.nest

import local.hull.bill.BillManager.ForanScrap

object CommonNest {

  case class NestIdBlock(nestID: String, block: String)

  case class NestMaterial(MATERIAL: String = "",
                          THICKNESS: Double = 0.0,
                          NEST_LENGTH: Int = 0,
                          NEST_WIDTH: Int = 0,
                          PARENTID: String = "",
                          GROSSWEIGHT: Int = 0
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
                   GROSSWEIGHT: Int = 0,
                   SCRAP: ForanScrap = ForanScrap()
                 )

  case class Nest2(
                    OID: Int = 0,
                    NESTID: String = "",
                    KPL: Int = 0,
                    LENGTH: Double = 0.0,
                    WIDTH: Double = 0.0,
                    THICKNESS: Double = 0.0,
                    MAT: String = "",
                    DENSITY: Double = 0.0,
                    BLOCKS: String = "",
                    NUMPRT: Int = 0,
                    NUMEQNEST: Int = 0,
                    AREA: Double = 0.0,
                    PARTSWEIGHT: Double = 0.0,
                    GROSSWEIGHT: Double = 0.0,
                    USAGE: Double = 0.0,
                    PARENTNESTID: String = "",
                    CHILDNESTID: String = "",
                    CHILDKPL: Int = 0,
                    CHILDLENGTH: Double = 0.0,
                    CHILDWIDTH: Double = 0.0,
                    CHILDAREAM2: Double = 0.0,
                    CHILDWEIGHT: Double = 0.0,
                    isLock: Boolean = false,
                    lockInfo: NestLock = NestLock()
                  )

  case class NestLock(project: String = "", nestId: String = "", date: Long = 0, user: String = "")


  def calculateWeightKG(Wmm: Double, Lmm: Double, Tmm: Double, ROgrammBySm3: Double): Int = Math.ceil(((Wmm / 10.0 * Lmm / 10.0 * Tmm / 10.0) * ROgrammBySm3) / 1000.0).toInt

}
