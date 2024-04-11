package deepsea.materials

import deepsea.database.DBManager
import deepsea.esp.EspManager.{GlobalEsp, GlobalEspSpec, IssueProject}
import deepsea.esp.EspManagerHelper
import deepsea.pipe.PipeManager.{Material, MaterialDirectory, MaterialDirectoryTable, MaterialStatement, MaterialStatementTable, MaterialTranslation, ProjectName, SpecMaterial, SpecMaterialTable}
import io.circe.syntax.EncoderOps
import local.common.DBRequests.MaterialNode
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.equal
import slick.lifted.TableQuery

import scala.language.postfixOps
import slick.jdbc.PostgresProfile.api._
import slick.lifted.{ProvenShape, TableQuery}

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, SECONDS}

trait MaterialsHelper {
  def getMaterials1: List[Material] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n").find[Material]().toFuture(), Duration(30, SECONDS)) match {
          case dbMaterials => dbMaterials.toList
          case _ => List.empty[Material]
        }
      case _ => List.empty[Material]
    }
  }
  def getMaterialNodes(project: String): List[MaterialNode] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        Await.result(mongo.getCollection("materials-n-nodes").find[MaterialNode](equal("project", project)).toFuture(), Duration(60, SECONDS)) match {
          case nodes => nodes.toList
          case _ => List.empty[MaterialNode]
        }
      case _ => List.empty[MaterialNode]
    }
  }
  def getProjects: List[ProjectName] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val projectNamesCollection: MongoCollection[ProjectName] = mongo.getCollection("project-names")
        Await.result(projectNamesCollection.find().toFuture(), Duration(30, SECONDS)) match {
          case values: Seq[ProjectName] => values.toList
          case _ => List.empty[ProjectName]
        }
      case _ => List.empty[ProjectName]
    }
  }
  def getMaterials: List[Material] = {
    val statements = (Await.result(DBManager.PostgresSQL.run(TableQuery[MaterialStatementTable].result).map(_.toList), Duration(5, SECONDS)) match {
      case response: List[MaterialStatement] => response
      case _ => List.empty[MaterialStatement]
    })
    val materials = (Await.result(DBManager.PostgresSQL.run(TableQuery[SpecMaterialTable].filter(_.removed === 0).result).map(_.toList), Duration(5, SECONDS)) match {
      case response: List[SpecMaterial] => response
      case _ => List.empty[SpecMaterial]
    })
    val projects = getSpecIssueProjects
    materials.map(m => {
      val statement = statements.find(_.id == m.statem_id) match {
        case Some(value) => value.code
        case _ => ""
      }
      val project = statements.find(_.id == m.statem_id) match {
        case Some(value) =>
          projects.find(_.id == value.project_id) match {
            case Some(proj) => proj.rkd
            case _ => ""
          }
        case _ => ""
      }
      Material(
        m.name,
        m.descr,
        statement,
        m.code,
        m.units,
        m.weight,
        project,
        "",
        m.supplier,
        m.note,
        "",
        m.manufacturer,
        m.coef,
        m.id.toString
      )
    })
  }
  def getSpecIssueProjects: ListBuffer[IssueProject] ={
    val res = ListBuffer.empty[IssueProject]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val rs = s.executeQuery(s"select * from issue_projects where status = 0 order by id")
        while (rs.next()){
          res += IssueProject(
            Option(rs.getInt("id")).getOrElse(0),
            Option(rs.getString("name")).getOrElse(""),
            Option(rs.getString("pdsp")).getOrElse(""),
            Option(rs.getString("rkd")).getOrElse(""),
            Option(rs.getString("foran")).getOrElse(""),
            Option(rs.getString("managers")).getOrElse(""),
            Option(rs.getString("status")).getOrElse(""),
            Option(rs.getString("factory")).getOrElse(""),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }
  def getMaterialStatements: List[MaterialStatement] = {
    (Await.result(DBManager.PostgresSQL.run(TableQuery[MaterialStatementTable].result).map(_.toList), Duration(5, SECONDS)) match {
      case response: List[MaterialStatement] => response
      case _ => List.empty[MaterialStatement]
    })
  }
  def getSpecMaterials: List[SpecMaterial] = {
    (Await.result(DBManager.PostgresSQL.run(TableQuery[SpecMaterialTable].filter(_.removed === 0).result).map(_.toList), Duration(5, SECONDS)) match {
      case response: List[SpecMaterial] => response
      case _ => List.empty[SpecMaterial]
    })
  }
  def getMaterialDirectories: List[MaterialDirectory] = {
    (Await.result(DBManager.PostgresSQL.run(TableQuery[MaterialDirectoryTable].filter(_.removed === 0).result).map(_.toList), Duration(5, SECONDS)) match {
      case response: List[MaterialDirectory] => response
      case _ => List.empty[MaterialDirectory]
    })
  }
}
