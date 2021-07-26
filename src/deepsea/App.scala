package deepsea

import deepsea.actors.ActorManager
import scala.io.StdIn.readLine

object App {
  object HTTPServer{
    val Host = "192.168.1.122"
    val Port = 1112
  }
  def main(args: Array[String]): Unit = {
    org.apache.log4j.BasicConfigurator.configure()
    ActorManager.init()
    while (readLine() != "q") {}
    ActorManager.terminate()
  }
}
