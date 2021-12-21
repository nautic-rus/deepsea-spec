package local.ele.eq

object EleEqManager extends EleEqHelper {

  case class EleEq(
                    OID: Int = 0,
                    TYPE: Int = 0,
                    USERID: String = "",
                    ZONE_SEQID: Int = 0,
                    ZONE_NAME: String = "",
                    ZONE_DESCR: String = "",
                    SYSTEM_SEQID: Int,
                    SYSTEM_NAME: String = "",
                    SYSTEM_DESCR: String = "",
                    ABBREV: String = "",
                    XCOG: Double = 0,
                    YCOG: Double = 0,
                    ZCOG: Double = 0,
                    WEIGHT: Double = 0,
                    STOCK_CODE: String = "",
                    CLASS_NAME: String = "",
                    SURFACE: String = "",
                    MARIGIN: Int = 0
                  )

  def genEqLabelsByEqOid(project: String, eqOid: String): List[String] = eqLabelsByEqOid(project, eqOid)

}
