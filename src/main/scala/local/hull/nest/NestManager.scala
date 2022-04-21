package local.hull.nest

import local.hull.nest.CommonNest.{Nest, Nest2, NestMaterial}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs

object NestManager extends NestHelper with Codecs {

  def genAllPlateNest2(project: String): List[Nest2] =allPlateNest2(project)
  def genAllPlateNest2Json(project: String): String = genAllPlateNest2(project).asJson.noSpaces

  def genAllPlateNest(project: String): List[Nest] = allPlateNest(project)

  def genAllPlateNestJson(project: String): String = genAllPlateNest(project).asJson.noSpaces

  def genMateriaAlllListJson(project: String): String = genMateriaAlllList(project).asJson.noSpaces

  def genMaterialNyBlockJson(project: String, blocks: List[String]): String = genMaterialByBlock(project, blocks).asJson.noSpaces

  def genBlocksJson(project: String): String = genBlocks(project).asJson.noSpaces

  def plateNestByMaterialsJson(project: String, materials: List[NestMaterial]): String = plateNestByMaterials(project, materials).asJson.noSpaces

  def plateNestByMaterialsAndDimsJson(project: String, materials: List[NestMaterial]): String = plateNestByMaterialsAndDims(project, materials).asJson.noSpaces

  def plateNestByBlocksJson(project: String, blocks: List[String]): String = plateNestByBlocks(project, blocks).asJson.noSpaces

}
