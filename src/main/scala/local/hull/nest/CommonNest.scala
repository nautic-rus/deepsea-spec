package local.hull.nest

object CommonNest {

  case class NestIdBlock(nestID: String, block: String)

  case class Nest(
                   ID:String="",
                   MATERIAL:String="",
                   THICKNESS:Double=0.0,
                   NEST_LENGTH:Double=0.0,
                   NEST_WIDTH:Double=0.0,
                   NUM_EQ_NEST:Int=0,
                   PARTS_WEIGHT:Double=0.0,
                   DENSITY:Double=0.0,
                   USAGE:Double=0.0,
                   BLOCKS:String=""
                 )

}
