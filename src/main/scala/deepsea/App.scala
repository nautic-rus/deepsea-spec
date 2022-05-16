package deepsea

import com.typesafe.config.{Config, ConfigFactory}
import deepsea.actors.ActorManager

import scala.io.StdIn.readLine

object App {

  val conf: Config = ConfigFactory.defaultApplication(this.getClass.getClassLoader)

  object HTTPServer{
    val Host = "192.168.1.28"
    val Port = 1113
    val Url = "https://deep-sea.ru"
    val RestUrl = "https://deep-sea.ru/rest"
  }
  object Cloud{
    val Host = "cloud.nautic-rus.ru"
    val Protocol = "https"
    val Directory = "/cloud"
    val Url: String = App.HTTPServer.RestUrl + "/files"
  }
  def main(args: Array[String]): Unit = {
    org.apache.log4j.BasicConfigurator.configure()
    //Logger.getLogger( "org.mongodb.driver" ).setLevel(Level.ERROR)
    ActorManager.init()
    while (readLine() != "q") {}
    ActorManager.terminate()
  }
}
