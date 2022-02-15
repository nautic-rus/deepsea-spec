package local.hull.nest

import local.hull.nest.CommonNest.Nest
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs

object NestManager extends NestHelper with Codecs{

  def genAllPlateNest(project:String):List[Nest]=allPlateNest(project)

  def genAllPlateNestJson(project:String)=genAllPlateNest(project).asJson.noSpaces


}
