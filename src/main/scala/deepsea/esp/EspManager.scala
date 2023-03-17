package deepsea.esp

import akka.actor.Actor
import deepsea.esp.EspManager.{CreateEsp, EspElement, EspObject, GetEsp}
import deepsea.pipe.PipeHelper
import deepsea.pipe.PipeManager.{Material, PipeSeg}
import io.circe.generic.JsonCodec
import io.circe.{Decoder, Encoder}
import io.circe.syntax.EncoderOps
import local.common.Codecs
import local.pdf.ru.common.ReportCommon.Item11Columns
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.syntax._
import local.hull.PartManager.{ForanPartsByDrawingNum, PrdPart}

import java.util.{Date, UUID}

object EspManager{

  case class EspObject(id: String, foranProject: String, docNumber: String, rev: String, date: Long, user: String, kind: String, elements: List[EspElement])

  trait EspElementClass
  case class EspElement(kind: String, prdPart: Option[PrdPart] = Option.empty[PrdPart], pipeSeg: Option[PipeSeg] = Option.empty[PipeSeg]){
    def getOption: Option[EspElementClass] = {
      kind match {
        case "hull" => prdPart
        case "pipe" => pipeSeg
        case _ => Option.empty[EspElementClass]
      }
    }
  }

  case class CreateEsp(foranProject: String, docNumber: String, rev: String, user: String, kind: String)
  case class GetEsp(docNumber: String, rev: String)
}

class EspManager extends Actor with EspManagerHelper with Codecs with PipeHelper{

  override def preStart(): Unit = {
    //val qw = getAllLatestEsp
    //val jk = qw
    //self ! CreateEsp("N002", "200101-222-104", "C", "isaev", "hull")
  }
  override def receive: Receive = {
    case CreateEsp(foranProject, docNumber, rev, user, kind) =>
      val id = UUID.randomUUID().toString
      val date = new Date().getTime
      val hullElements = kind match {
        case "hull" =>
          ForanPartsByDrawingNum(foranProject, docNumber).map(x => EspElement("hull", prdPart = Option(x)))
        case "pipe" =>
          val projectSystem = getSystemAndProjectFromDocNumber(docNumber)
          val pipeSegs = getPipeSegs(projectSystem._1, projectSystem._2)
          pipeSegs.map(x => EspElement("pipe", pipeSeg = Option(x)))
        case _ => List.empty[EspElement]
      }
      val esp = EspObject(id, foranProject, docNumber, rev, date, user, kind, hullElements)
      addEsp(esp)
      sender() ! "success".asJson.noSpaces
    case GetEsp(docNumber, rev) =>
      sender() ! getAllLatestEsp.asJson.noSpaces
      //sender() ! getLatestEsp(docNumber, rev).asJson.noSpaces
    case _ => None
  }
}
