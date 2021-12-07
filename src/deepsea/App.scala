package deepsea

import deepsea.actors.ActorManager
import org.apache.log4j.{Level, Logger}

import scala.io.StdIn.readLine

object App {
  object HTTPServer{
    val Host = "192.168.1.122"
    val Port = 1113
    val Url = "https://deep-sea.ru"
    val RestUrl = "https://deep-sea.ru/rest"
  }
  object Cloud{
    val Host = "cloud.nautic-rus.ru"
    val Protocol = "https"
    val Directory = "D:/cloud"
    val Url: String = App.HTTPServer.RestUrl + "/files"
  }
  def main(args: Array[String]): Unit = {
    org.apache.log4j.BasicConfigurator.configure()
    ActorManager.init()
    while (readLine() != "q") {}
    ActorManager.terminate()
  }
}
