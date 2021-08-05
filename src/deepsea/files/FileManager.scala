package deepsea.files

import akka.actor.Actor
import deepsea.App
import deepsea.files.FileManager.CreateFile
import deepsea.files.classes.FileAttachment
import org.aarboard.nextcloud.api.NextcloudConnector
import org.aarboard.nextcloud.api.filesharing.{SharePermissions, ShareType}
import play.api.libs.json.Json

import java.io.InputStream
import java.util.UUID

object FileManager{
  case class CreateFile(fileName: String, stream: InputStream)
}
class FileManager extends Actor{
  override def receive: Receive = {
    case CreateFile(fileName, stream) =>
      sender() ! Json.toJson(new FileAttachment(fileName, uploadFile(fileName, stream)))
    case _ => None
  }
  def uploadFile(fileName: String, stream: InputStream): String ={
    val nextCloud = new NextcloudConnector("cloud.nautic-rus.ru", true, 443, "files", "Ship1234")
    var path = UUID.randomUUID.toString
    while (nextCloud.folderExists(path)){
      path = UUID.randomUUID.toString
    }
    nextCloud.createFolder(path)
    nextCloud.uploadFile(stream, path + "/" + fileName)
    val shareToken = nextCloud.doShare(path + "/" + fileName, ShareType.PUBLIC_LINK, "", false, "", new SharePermissions(0)).getToken
    App.Cloud.Protocol + "://" + App.Cloud.Host + "/s/" + shareToken + "/download/" + fileName
  }
}
