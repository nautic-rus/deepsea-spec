package local.domain

import org.bson.Document

import java.util.UUID
import scala.collection.mutable.ListBuffer

class WorkShopMaterial
(
  var id: String,
  val project: String,
  val name: String,
  val description: String,
  val category: String,
  val trmCode: String,
  val sfiCode: String,
  val units: String,
  val singleWeight: Double,
  val document: String,
  val provider: String,
  val note: String,
  val comment: String,
  val files: List[String],
  val coefficient: Double,

) extends Serializable {
  //var id: String = UUID.randomUUID().toString
  var version: Int = 0
  var removed: Int = 0
  var fixedCount: Int = 0


}


