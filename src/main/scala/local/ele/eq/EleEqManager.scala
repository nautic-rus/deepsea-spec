package local.ele.eq

object EleEqManager extends EleEqHelper {
  def genEqLabelsByEqOid(project: String, eqOid: String): List[String] = eqLabelsByEqOid(project, eqOid)

}
