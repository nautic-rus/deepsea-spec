package deepsea.files

import akka.actor.Actor
import deepsea.App
import deepsea.files.FileManager.{CloudFile, CreateFile, GenerateUrl}

import java.io.{File, FileOutputStream, FileWriter}
import java.nio.file.{Files, Path}
import java.util.{Date, UUID}


object FileManager{
  case class CreateFile(prefix: String, ext: String)
  case class CloudFile(path: String, url: String, file: File)
  case class GenerateUrl(path: String)
}
class FileManager extends Actor{
  override def receive: Receive = {
    case CreateFile(prefix, ext) =>
      val fileName = prefix + "-" + new Date().getTime + "." + ext
      var pathId = UUID.randomUUID().toString.substring(0, 8)
      var file = new File(App.Cloud.Directory + "/" + pathId)
      while (file.exists()){
        pathId = UUID.randomUUID().toString.substring(0, 8)
        file = new File(App.Cloud.Directory + "/" + pathId)
      }
      file.mkdir()
      val filePath = App.Cloud.Directory + "/" + pathId + "/" + fileName
      val fileUrl = App.Cloud.Url + "/" + pathId + "/" + fileName
      file = new File(filePath)
      sender() ! CloudFile(filePath, fileUrl, file)
    case GenerateUrl(filePath: String) =>
      val source = new File(filePath)
      var pathId = UUID.randomUUID().toString.substring(0, 8)
      var file = new File(App.Cloud.Directory + "/" + pathId)
      while (file.exists()){
        pathId = UUID.randomUUID().toString.substring(0, 8)
        file = new File(App.Cloud.Directory + "/" + pathId)
      }
      file.mkdir()
      val cloudPath = App.Cloud.Directory + "/" + pathId + "/" + source.getName
      Files.write(new File(cloudPath).toPath, Files.readAllBytes(source.toPath))
      val fileUrl = App.Cloud.Url + "/" + pathId + "/" + source.getName
      sender() ! fileUrl
  }
}
