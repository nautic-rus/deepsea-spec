package local.ele.eq

import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer

trait EleEqHelper {
  private def eqLabelSql(eqOid: String) = s"select  (select long_descr from ELEMENT_LANG where lang=-1 and elem = oid) as EQELEC from element where oid=${eqOid}"

  def eqLabelsByEqOid(project: String, eqOid: String): List[String] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(eqLabelSql(eqOid))
          if (rs.next()) {
            extractEQLabels(Option[String](rs.getString("EQELEC")).getOrElse(""))
          } else {
            List("NF")
          }
        }
        catch {
          case _: Throwable => List("NF")
        }
      }
      case None => List("NF")
    }
  }

  private def extractEQLabels(in: String): List[String] = {
    if (in.nonEmpty) {
      if (in.contains('\n')) {
        val buff = ListBuffer.empty[String]
        in.split('\n').foreach(line => {
          if (line.contains('|')) {
            buff += line.split('|').headOption.getOrElse("NF")
          } else {
            buff += line
          }
        })
        buff.toList
      } else {
        List(in)
      }
    } else {
      List("NF")
    }
  }
}
