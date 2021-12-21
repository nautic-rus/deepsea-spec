package deepsea.hull.classes

import play.api.libs.json.{JsNull, JsValue, Json, Writes}

object HullPart{
  implicit val writesIssue: Writes[HullPart] = new Writes[HullPart] {
    override def writes(o: HullPart): JsValue = o match {
      case x: HullPart => Json.obj(
        "PARTOID" -> x.PARTOID,
        "PARTNAME" -> x.PARTNAME,
        "DESCRIPTION" -> x.DESCRIPTION,
        "PART_TYPE" -> x.PART_TYPE,
        "BLOCKNAME" -> x.BLOCKNAME,
        "WEIGHT" -> x.WEIGHT,
        "X_COG" -> x.X_COG,
        "Y_COG" -> x.Y_COG,
        "Z_COG" -> x.Z_COG,
        "BLOCK_DESCRIPTION" -> x.BLOCK_DESCRIPTION,
        "KSE" -> x.KSE,
        "THICK" -> x.THICK,
        "MATERIAL" -> x.MATERIAL,
        "SECTION" -> x.SECTION,
        "PARENT_NODE" -> x.PARENT_NODE,
        "ATOM_TYPE" -> x.ATOM_TYPE,
        "ORDINAL" -> x.ORDINAL,
        "TYPE_ATOM_TYPE" -> x.TYPE_ATOM_TYPE,
        "SYMBOL" -> x.SYMBOL,
        "LINE_NAME" -> x.LINE_NAME,
        "LINE_TYPE" -> x.LINE_TYPE,
        "LINE_TYPE_DESCRIPTION" -> x.LINE_TYPE_DESCRIPTION,
        "BS_ADN" -> x.BS_ADN,
        "STOCK_CODE" -> x.STOCK_CODE,
      )
      case _ => JsNull
    }
  }

}
class HullPart(val PARTOID: Int, val PARTNAME: String, val PART_TYPE: Int, val DESCRIPTION: String, val BLOCKNAME: String, val WEIGHT: Double,
               val X_COG: Double, val Y_COG: Double, val Z_COG: Double, val BLOCK_DESCRIPTION: String, val KSE: Int, var THICK: Double = 0) {
  var MATERIAL: String = ""
  var SECTION: String = ""
  var PARENT_NODE: Int = 0
  var ATOM_TYPE: Int = 0
  var ORDINAL: Int = 0
  var TYPE_ATOM_TYPE: Int = 0
  var SYMBOL: String = ""
  var LINE_NAME: String = ""
  var LINE_TYPE: String = ""
  var LINE_TYPE_DESCRIPTION: String = ""
  var BS_ADN: String = ""
  var STOCK_CODE: String = ""
}
