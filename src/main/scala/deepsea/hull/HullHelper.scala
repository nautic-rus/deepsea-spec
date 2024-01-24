package deepsea.hull

import deepsea.database.DBManager
import deepsea.hull.HullManager.IssueMaterial
import deepsea.pipe.PipeManager.Material
import local.hull.PartManager.PrdPart

import java.util.Date
import scala.collection.mutable.ListBuffer

trait HullHelper {

  def addMaterial(pos: String, units: String, weight: Double, count: Double, stock: String, userId: Int, docNumber: String, issueId: Int, addText: String, department: String): Unit = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val d = new Date().getTime
        s.execute(s"insert into issue_materials (pos, units, weight, count, material_stock_code, user_id, date_inserted, doc_number, issue_id, add_text, department) values ('$pos', '$units', $weight, $count, '$stock', $userId, $d, '$docNumber', $issueId, '$addText', '$department')")
        s.close()
        c.close()
      case _ => None
    }
  }

  def deleteMaterial(id: Int): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"delete from issue_materials where id = $id")
        s.close()
        c.close()
      case _ => None
    }
    "success"
  }

  def deleteMaterial(pos: String, docNumber: String, department: String): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        s.execute(s"delete from issue_materials where pos = '$pos' and doc_number = '$docNumber' and department = '$department'")
        s.close()
        c.close()
      case _ => None
    }
    "success"
  }

  def getIssueMaterials(issueId: Int): List[IssueMaterial] = {
    val res = ListBuffer.empty[IssueMaterial]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_materials where issue_id = $issueId")
        while (rs.next()) {
          res += IssueMaterial(
            rs.getInt("id"),
            rs.getString("pos"),
            rs.getString("units"),
            rs.getDouble("weight"),
            rs.getDouble("count"),
            rs.getString("material_stock_code"),
            rs.getInt("user_id"),
            rs.getLong("date_inserted"),
            rs.getString("doc_number"),
            rs.getInt("issue_id"),
            rs.getString("add_text"),
            rs.getString("department")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ => None
    }
    res.toList
  }

  def getIssueMaterials(docNumber: String): List[IssueMaterial] = {
    val res = ListBuffer.empty[IssueMaterial]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_materials where doc_number = '$docNumber'")
        while (rs.next()) {
          res += IssueMaterial(
            rs.getInt("id"),
            rs.getString("pos"),
            rs.getString("units"),
            rs.getDouble("weight"),
            rs.getDouble("count"),
            rs.getString("material_stock_code"),
            rs.getInt("user_id"),
            rs.getLong("date_inserted"),
            rs.getString("doc_number"),
            rs.getInt("issue_id"),
            rs.getString("add_text"),
            rs.getString("department")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ => None
    }
    res.toList
  }

  def getHullIssueMaterials(issueId: Int, materials: List[Material]): List[PrdPart] = {
    getIssueMaterials(issueId).map(_.toHullPart(materials))
  }

  def getHullIssueMaterials(docNumber: String, materials: List[Material]): List[PrdPart] = {
    getIssueMaterials(docNumber).map(_.toHullPart(materials))
  }
}
