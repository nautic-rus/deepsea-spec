package local.domain
import java.util.UUID

class WorkShopMaterial
(
  var project: String="",
  val name: String="",
  val description: String="",
  val category: String="",
  var trmCode: String="",
  val trmCodeFactory: String="",
  val sfiCode: String="",
  val units: String="",
  val singleWeight: Double=0,
  val document: String="",
  val provider: String="",
  val note: String="",
  val comment: String="",
  val files: List[String]=List.empty[String],
){
  var id: String = UUID.randomUUID().toString
  var version: Int = 0
  var removed: Int = 0
  var coefficient: Double = 1
}


