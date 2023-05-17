import local.ele.CommonEle
import local.ele.CommonEle.retrieveEleComplects
import local.ele.eq.EleEqHelper
import local.ele.eq.EleEqManager.{genEqByOIDJson, genEqLabelsByEqOidJson, genEqsByComplectJson}
import org.apache.log4j.{Level, Logger}
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer

class EqTest extends AnyFunSuite with EleEqHelper {
  org.apache.log4j.BasicConfigurator.configure()
  Logger.getLogger("org.mongodb.driver").setLevel(Level.ERROR)


  private def calculateDrawingLabelOld(in: String): String = {
    if (in.nonEmpty) {
      if (in.contains('\n')) {
        val fr = in.split('\n').head
        if (fr.contains('|')) {
          fr.split('|').head
        } else {
          fr
        }
      } else {
        if (in.contains('|')) {
          in.split('|').head
        } else {
          in
        }
      }
    } else {
      "NF"
    }
  }


  private def calculateDrawingLabel(in: String): List[String] = {
    val buff = ListBuffer.empty[String]
    if (in.nonEmpty) {
      if (in.contains('\n')) {
        val fr = in.split('\n')
        fr.foreach(line => {
          if (line.contains('|')) {
            val label = line.split('|').head
            if (!label.contains(".")) buff += label

          } else {
            buff += line
          }
        })
      }
      else {
        if (in.contains('|')) {
          buff += in.split('|').head
        } else {
          buff += in
        }
      }
    } else {
      buff += "NF"
    }
    buff.toList
  }

  //val testStr = "46\n46.2|0188W0004009A2K|796|4\n46.3|0199W1300050A2K|796|4\n46.4|NR12F37B7D2BBE37|796|4\n46.58|0192W0001201A2K|796|4"

 //val demo: List[String] = calculateDrawingLabel(testStr)

  //4105502



   val eqLabel: String = genEqLabelsByEqOidJson("P701", "4105502")

  val vv=0

  //val eqJson: String =genEqByOIDJson("P701", "7119738")

  //val complects: CommonEle.EleComplect =retrieveEleComplects("P701").find(s=>s.drawingId.equals("170701-884-6009")).getOrElse(null)

  //eqByOID

  val kk = 0


}
