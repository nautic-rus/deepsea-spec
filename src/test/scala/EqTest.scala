import local.ele.eq.EleEqHelper
import org.apache.log4j.{Level, Logger}
import org.scalatest.funsuite.AnyFunSuite

class EqTest extends AnyFunSuite  with EleEqHelper{
  org.apache.log4j.BasicConfigurator.configure()
  Logger.getLogger( "org.mongodb.driver" ).setLevel(Level.ERROR)


  eqByOID("P701", "7119738")


}
