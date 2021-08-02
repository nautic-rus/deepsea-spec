package deepsea.files

import akka.actor.Actor
import deepsea.App
import deepsea.files.FileManager.CreateFile
import org.aarboard.nextcloud.api.NextcloudConnector
import org.aarboard.nextcloud.api.filesharing.{SharePermissions, ShareType}
import play.api.libs.json.Json

import java.io.InputStream

object FileManager{
  case class CreateFile(fileName: String, stream: InputStream)
}
class FileManager extends Actor{
  override def receive: Receive = {
    case CreateFile(fileName, stream) =>
      sender() ! Json.toJson(uploadFile(fileName, stream))
    case _ => None
  }
  def uploadFile(fileName: String, stream: InputStream): String ={
    val nextCloud = new NextcloudConnector("cloud.nautic-rus.ru", true, 443, "files", "Ship1234")
    nextCloud.uploadFile(stream, fileName)
    val shareToken = nextCloud.doShare(fileName, ShareType.PUBLIC_LINK, "", false, "", new SharePermissions(0)).getToken
    App.Cloud.Protocol + "://" + App.Cloud.Host + "/s/" + shareToken + "/download/" + fileName
  }
}
