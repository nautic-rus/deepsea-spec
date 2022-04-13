package local.hull.nest

object CommonNest {

  case class NestIdBlock(nestID: String, block: String)

  case class NestMaterial(MATERIAL: String = "",
                          THICKNESS: Double = 0.0,
                          NEST_LENGTH: Int = 0,
                          NEST_WIDTH: Int = 0,
                          PARENTID: String = ""
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
                   PARENTID: String = ""
                 )

  case class NestLock(project: String = "", nestId: String = "", date: Long = 0, user: String = "")

}
