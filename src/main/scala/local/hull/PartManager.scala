package local.hull

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs

object PartManager extends PartHelper with Codecs {

  case class PrdPart(
                      PART_OID: Int = 0,
                      EXPL_OID: Int = 0,
                      PART_CODE: String = "",
                      SYMMETRY: String = "",
                      PART_TYPE: Int = 0,
                      BLOCK_CODE: String = "",
                      DESCRIPTION: String = "",
                      NUM_EQ_PART: Int = 0,
                      PART_DESC: String = "",
                      ELEM_TYPE: String = "",
                      MATERIAL: String = "",
                      LENGTH: Double = 0.0,
                      WIDTH: Double = 0.0,
                      THICKNESS: Double = 0.0,
                      WEIGHT_UNIT: Double = 0.0,
                      TOTAL_WEIGHT: Double = 0.0,
                      NEST_ID: String = "",
                      NEST_LENGTH: Double = 0.0,
                      NEST_WIDTH: Double = 0.0,
                      NUM_EQ_NEST: Int = 0,
                      WH: Double = 0.0,
                      WT: Double = 0.0,
                      FH: Double = 0.0,
                      FT: Double = 0.0,
                      STRGROUP:String=""
                    )


  def genForanPartLabelByDrawingNumAndPartNameJSON(project: String, drNum: String, partName: String): String = {
    ForanPartLabelByDrawingNumAndPartName(project, drNum, partName).asJson.noSpaces
  }

  def genForanPartsByDrawingNum(project: String, drNum: String): List[PrdPart] = ForanPartsByDrawingNum(project, drNum)

  def genForanPartsByDrawingNumJSON(project: String, drNum: String): String = {
    ForanPartsByDrawingNum(project, drNum).sortBy(s => s.PART_CODE).asJson.noSpaces
  }
}
