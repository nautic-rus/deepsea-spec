package deepsea

import com.typesafe.config.{Config, ConfigFactory}
import deepsea.actors.ActorManager
import org.apache.log4j.{Level, Logger}

import scala.io.StdIn.readLine

object App {

  val conf: Config = ConfigFactory.defaultApplication(this.getClass().getClassLoader())

  object HTTPServer{
    val Host = "192.168.1.122"
    val Port = 1113
  }
  object Cloud{
    val Host = "cloud.nautic-rus.ru"
    val Protocol = "https"
    val Directory = "D:/cloud"
    val Url = "https://deepseadebug.nautic-rus.ru/rest/files"
  }
  def main(args: Array[String]): Unit = {
    org.apache.log4j.BasicConfigurator.configure()
    ActorManager.init()
    while (readLine() != "q") {}
    ActorManager.terminate()
  }
}
