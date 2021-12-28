import local.ele.CommonEle
import local.ele.CommonEle.{  retrieveEleComplects}
import local.ele.eq.EleEqHelper
import local.ele.eq.EleEqManager.{genEqByOIDJson, genEqLabelsByEqOidJson, genEqsByComplectJson}
import org.apache.log4j.{Level, Logger}
import org.scalatest.funsuite.AnyFunSuite

class EqTest extends AnyFunSuite  with EleEqHelper{
  org.apache.log4j.BasicConfigurator.configure()
  Logger.getLogger( "org.mongodb.driver" ).setLevel(Level.ERROR)

  //val eqLabel: String =genEqLabelsByEqOidJson("P701", "7119738")


  //val eqJson: String =genEqByOIDJson("P701", "7119738")

  //val complects: CommonEle.EleComplect =retrieveEleComplects("P701").find(s=>s.drawingId.equals("170701-884-6009")).getOrElse(null)

  //eqByOID

  val kk=0


}
