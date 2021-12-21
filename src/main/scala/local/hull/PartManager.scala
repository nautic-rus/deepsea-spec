package local.hull

import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps

object PartManager extends PartHelper {
  case class PartLabel(
                        PART_CODE: String = "",
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
                        NUM_PART_NEST: Int = 0
                      )

  case class PrdPart(
                      OID: Int = 0,
                      PART_CODE: String = "",
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
                      NUM_PART_NEST: Int = 0,
                      NEST_LENGTH: Double = 0.0,
                      NEST_WIDTH: Double = 0.0,
                      NUM_EQ_NEST: Int = 0,
                      KSE_KPL: Int = 0,
                      STOCKCODE: String = ""
                    )


  implicit val PartLabelItemDecoder: Decoder[PartLabel] = deriveDecoder[PartLabel]
  implicit val TrayMountItemEncoder: Encoder[PartLabel] = deriveEncoder[PartLabel]

  implicit val PrdPartDecoder: Decoder[PrdPart] = deriveDecoder[PrdPart]
  implicit val PrdPartEncoder: Encoder[PrdPart] = deriveEncoder[PrdPart]


  def genForanPartLabelByDrawingNumAndPartName(project: String, drNum: String, partName: String): PartLabel = ForanPartLabelByDrawingNumAndPartName(project, drNum, partName)

  def genForanPartLabelByDrawingNumAndPartNameJSOB(project: String, drNum: String, partName: String): String = {
    ForanPartLabelByDrawingNumAndPartName(project, drNum, partName).asJson.noSpaces
  }

  def genForanPartsByDrawingNum(project: String, drNum: String): List[PrdPart] = ForanPartsByDrawingNum(project, drNum)

  def genForanPartsByDrawingNumJSON(project: String, drNum: String): String = {
    ForanPartsByDrawingNum(project, drNum).sortBy(s=>s.PART_CODE).asJson.noSpaces
  }
}
