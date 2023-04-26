package deepsea.spec.classes

import java.util.Date

trait PartList {
  var user: String
  var docNumber: String
  var revision: String
  var date: Long
  def setRevision(user: String, revision: String): Unit ={
    this.user = user
    this.revision = revision
    date = new Date().getTime
  }
}
