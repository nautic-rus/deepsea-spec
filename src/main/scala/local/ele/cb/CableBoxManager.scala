package local.ele.cb


import io.circe._
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import local.common.Codecs
import local.common.DBRequests.{findWorkshopMaterial, findWorkshopMaterialContains, retrieveAllMaterialsByProject}
import local.domain.WorkShopMaterial

import scala.collection.mutable.ListBuffer

object CableBoxManager extends CableBoxHelper with Codecs{
  case class ForanCableBox(
                            PIDSQ: Int = 0,
                            USERID:String="",
                            X_COG: Double = 0,
                            Y_COG: Double = 0,
                            Z_COG: Double = 0,
                            WEIGHT: Double = 0,
                            NODE1: Int = 0,
                            NODE2: Int = 0,
                            TYPE: Int = 0,
                            SEAL_TYPE: String = "",
                            CODE: String = "",
                            DESCR: String = "",
                            STOCK_CODE: String = "",
                            PENRTRATION: String = ""
                          )

  case class CableBox(foranData: ForanCableBox, workshopData: WorkShopMaterial)



  def cableBoxBySeqId(project: String, cbIdSeq: String):CableBox={
    val wsmats=retrieveAllMaterialsByProject(project)
    val fcb=ForanCableBoxBySeqId(project, cbIdSeq)
    CableBox(fcb,findWorkshopMaterialContains(fcb.STOCK_CODE,wsmats))
  }

  def cableBoxBySeqIdJson(project: String, cbIdSeq: String):String={
    cableBoxBySeqId(project, cbIdSeq).asJson.noSpaces
  }
}
