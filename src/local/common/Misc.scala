package local.common
import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._


trait Misc {


  def genProjectList():String=List[String]("N002","P701").asJson.noSpaces
}
