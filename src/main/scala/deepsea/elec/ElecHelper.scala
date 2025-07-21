package deepsea.elec

import breeze.linalg.DenseMatrix
import cats.effect.unsafe.implicits.global
import cats.implicits._
import deepsea.database.DBManager
import deepsea.database.DBManager.foranProjects
import deepsea.elec.ElecManager._
import deepsea.esp.EspManager.EleEspObject
import deepsea.esp.EspManagerHelper
import deepsea.hull.HullManager.IssueMaterial
import deepsea.materials.MaterialsHelper
import deepsea.pipe.PipeManager.{Material, Pls, PlsParam}
import doodle.core._
import doodle.core.format._
import doodle.image.syntax.all._
import doodle.java2d._
import io.circe.parser._
import local.common.Codecs
import local.common.DBRequests.{MountItem, findWorkshopMaterialContains, retrieveAllMaterialsByProject}
import local.domain.WorkShopMaterial
import local.ele.CommonEle.EleComplect
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.{empty, equal}
import cats.implicits._
import cats.effect.unsafe.implicits.global
import com.itextpdf.io.font.{FontProgramFactory, PdfEncodings}
import com.itextpdf.io.image.ImageDataFactory
import com.itextpdf.kernel.font.PdfFontFactory.EmbeddingStrategy
import com.itextpdf.kernel.font.{PdfFont, PdfFontFactory}
import com.itextpdf.kernel.pdf.{PdfDocument, PdfWriter}
import com.itextpdf.layout.Document
import com.itextpdf.layout.borders.Border
import com.itextpdf.layout.element.{AreaBreak, Cell, Paragraph, Table}
import com.itextpdf.layout.properties.{HorizontalAlignment, TextAlignment}
import deepsea.App
import doodle.image.Image

import java.awt.geom.Area
import java.io.{File, FileOutputStream}
import java.nio.file.Files
import java.util.zip.{ZipEntry, ZipOutputStream}
import java.util.{Date, UUID}
import javax.swing.text.StyleConstants.FontConstants
import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source

trait ElecHelper extends Codecs with EspManagerHelper with MaterialsHelper {
  def getCablesInfo(project: String): List[ElecCable] = {
    val cables = ListBuffer.empty[ElecCable]
    DBManager.GetOracleConnection(project) match {
      case Some(oracleConnection) =>
        val stmt = oracleConnection.createStatement()
        val query = "SELECT * FROM V_CABLE"
        val rs = stmt.executeQuery(query)
        while (rs.next()) {
          cables += ElecCable(
            Option(rs.getString("CABLE_ID")).getOrElse(""),
            Option(rs.getString("FROM_E_ID")).getOrElse(""),
            Option(rs.getString("FROM_E_DESCR")).getOrElse(""),
            Option(rs.getString("TO_E_ID")).getOrElse(""),
            Option(rs.getString("TO_E_DESCR")).getOrElse(""),
            Option(rs.getString("SEGREGATION")).getOrElse(""),
            Option(rs.getString("NOM_SECTION")).getOrElse(""),
            Option(rs.getString("CABLE_SPEC")).getOrElse(""),
            Option(rs.getString("CAB_TYPE")).getOrElse(""),
            Option(rs.getString("SYSTEM")).getOrElse(""),
            Option(rs.getString("SYSTEM_DESCR")).getOrElse(""),
            Option(rs.getString("USER_MOD")).getOrElse(""),
            Option(rs.getString("FROM_E_ZONE_NAME")).getOrElse(""),
            Option(rs.getString("FROM_E_ZONE_DESCR")).getOrElse(""),
            Option(rs.getString("TO_E_ZONE_NAME")).getOrElse(""),
            Option(rs.getString("TO_E_ZONE_DESCR")).getOrElse(""),
            Option(rs.getString("F_ROUT")).getOrElse(""),
          )
        }
        rs.close()
        stmt.close()
        oracleConnection.close()
        cables.toList
      case _ =>
    }
    cables.toList
  }


  def getElecAngles: List[ElecAngle] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val elecAnglesCollection: MongoCollection[ElecAngle] = mongo.getCollection("elec-angles")
        Await.result(elecAnglesCollection.find[ElecAngle]().toFuture(), Duration(30, SECONDS)) match {
          case elecAngles => elecAngles.toList
          case _ => List.empty[ElecAngle]
        }
      case _ => List.empty[ElecAngle]
    }
  }

  def getTraysBySystem(project: String, docNumber: String): List[TraysBySystem] = {
    val res = ListBuffer.empty[TraysBySystem]
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val doc = docNumber.split("-").takeRight(2).mkString("-")
        val query = Source.fromResource("queries/elecTraysInSystem.sql").mkString.replaceAll(":docNumber", "'" + doc + "'")
        val s = c.prepareStatement(query)
        val rs = s.executeQuery()
        val projects = getIssueProjects
        val rkdProject = projects.find(_.foran == project) match {
          case Some(value) => value.rkd
          case _ => ""
        }
        val materials: List[Material] = getMaterials.filter(_.project == rkdProject)
        while (rs.next()) {
          val code = Option(rs.getString("STOCK_CODE")).getOrElse("");
          res += TraysBySystem(
            Option(rs.getString("SYSTEM")).getOrElse(""),
            Option(rs.getInt("OID")).getOrElse(0),
            Option(rs.getString("ZONE")).getOrElse(""),
            Option(rs.getInt("LINE")).getOrElse(0),
            Option(rs.getDouble("WEIGHT")).getOrElse(0.0),
            Option(rs.getDouble("X_COG")).getOrElse(0.0),
            Option(rs.getDouble("Y_COG")).getOrElse(0.0),
            Option(rs.getDouble("Z_COG")).getOrElse(0.0),
            Option(rs.getString("CTYPE")).getOrElse(""),
            Option(rs.getInt("TYPE")).getOrElse(0),
            code,
            Option(rs.getString("TRAY_DESC")).getOrElse(""),
            Option(rs.getString("NODE_1")).getOrElse(""),
            Option(rs.getDouble("N1_X")).getOrElse(0.0),
            Option(rs.getDouble("N1_Y")).getOrElse(0.0),
            Option(rs.getDouble("N1_Z")).getOrElse(0.0),
            Option(rs.getString("NODE_2")).getOrElse(""),
            Option(rs.getDouble("N2_X")).getOrElse(0.0),
            Option(rs.getDouble("N2_Y")).getOrElse(0.0),
            Option(rs.getDouble("N2_Z")).getOrElse(0.0),
            Option(rs.getDouble("LENGTH")).getOrElse(0.0),
            materials.find(x => x.code == code) match {
              case Some(value) => value
              case _ => Material()
            }
          )
        }
        rs.close()
        s.close()
        c.close()


        //todo - here goes some stuff needs to be edited
        val elecAngles = getElecAngles
        val anglesRes = ListBuffer.empty[TraysBySystem]
        res.groupBy(_.trayDesc).foreach(gr => {
          val elecAngle = elecAngles.find(a => gr._1.contains(a.name)) match {
            case Some(angleValue) =>
              materials.find(x => x.code == angleValue.code) match {
                case Some(material) => material
                case _ => Material()
              }
            case _ => Material()
          }
          if (elecAngle.code != "") {

            val len = gr._2.map(_.length).sum
            val totalHeight = 0.3 * Math.ceil(len / 1.2) * 2
            val wght: Double = totalHeight * elecAngle.singleWeight
            val totalWeight: String = if (wght < 0.01) " 0.01" else String.format("%.2f", wght)

            anglesRes += TraysBySystem(
              doc,
              0,
              "",
              0,
              totalWeight.toDoubleOption.getOrElse(0.0),
              0.0,
              0.0,
              0.0,
              "ANGLE",
              0,
              elecAngle.code,
              "",
              "",
              0.0,
              0.0,
              0.0,
              "",
              0.0,
              0.0,
              0.0,
              totalHeight,
              elecAngle
            )
          }
        })

        anglesRes.groupBy(_.stockCode).foreach(gr => {
          res += TraysBySystem(
            doc,
            0,
            "",
            0,
            gr._2.map(_.weight).sum,
            0.0,
            0.0,
            0.0,
            "ANGLE",
            0,
            gr._1,
            "",
            "",
            0.0,
            0.0,
            0.0,
            "",
            0.0,
            0.0,
            0.0,
            gr._2.map(_.length).sum,
            gr._2.head.material
          )
        })

      //todo - here it finished

      //        val angleCode: String = "MTLESNSTLXXX0047";
      //        val angle: Material = materials.find(x => x.code == angleCode) match {
      //          case Some(value) => value
      //          case _ => Material()
      //        }
      //
      //        var totalHeight: Double = 0.0;
      //        res.groupBy(x => x.stockCode).foreach(gr => {
      //          val len = gr._2.map(_.length).sum;
      //          totalHeight += 0.3 * Math.ceil(len / 1.2) * 2;
      //        })
      //        val wght: Double = totalHeight * angle.singleWeight;
      //        val totalWeight: String = if (wght < 0.01) " 0.01" else String.format("%.2f", wght);
      //
      //        res += new TraysBySystem(
      //          doc,
      //          0,
      //          "",
      //          0,
      //          totalWeight.toDoubleOption.getOrElse(0.0),
      //          0.0,
      //          0.0,
      //          0.0,
      //          "",
      //          0,
      //          angleCode,
      //          "",
      //          "",
      //          0.0,
      //          0.0,
      //          0.0,
      //          "",
      //          0.0,
      //          0.0,
      //          0.0,
      //          totalHeight,
      //          angle
      //        )

      case _ =>
    }
    res.toList
  }

  def getTotalTraysBySystem(project: String, docNumber: String): List[TrayBySystem] = {
    val res = ListBuffer.empty[TrayBySystem];
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val query = Source.fromResource("queries/elecTotalTraysInSystem.sql").mkString.replaceAll(":docNumber", "'" + docNumber + "'");
        val s = c.prepareStatement(query);
        val rs = s.executeQuery();
        while (rs.next()) {
          res += new TrayBySystem(
            rs.getString("SYSTEM"),
            rs.getString("STOCK_CODE"),
            rs.getString("TRAY_DESC"),
            rs.getDouble("LENGTH"),
            rs.getDouble("WEIGHT")
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res.toList
  }

  def getCableBoxesBySystem(project: String, docNumber: String): List[CableBoxesBySystem] = {
    val res = ListBuffer.empty[CableBoxesBySystem];
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val doc = docNumber.split("-").takeRight(2).mkString("-")
        val query = Source.fromResource("queries/elecTotalCableBoxesInSystem.sql").mkString.replaceAll(":docNumber", "'" + doc + "'");
        val s = c.prepareStatement(query);
        val rs = s.executeQuery();
        val materials: List[Material] = getMaterials
        try {
          while (rs.next()) {
            val code = Option(rs.getString("STOCK_CODE")).getOrElse("");
            val name = Option(rs.getString("CODE")).getOrElse("");
            res += new CableBoxesBySystem(
              Option(rs.getString("SYSTEM")).getOrElse(""),
              Option(rs.getString("USERID")).getOrElse(""),
              Option(rs.getInt("OID")).getOrElse(0),
              Option(rs.getString("ZONE")).getOrElse(""),
              Option(rs.getInt("LINE")).getOrElse(0),
              Option(rs.getDouble("WEIGHT")).getOrElse(0.0),
              Option(rs.getDouble("X_COG")).getOrElse(0.0),
              Option(rs.getDouble("Y_COG")).getOrElse(0.0),
              Option(rs.getDouble("Z_COG")).getOrElse(0.0),
              Option(rs.getString("SEAL_TYPE")).getOrElse(""),
              Option(rs.getInt("TYPE")).getOrElse(0),
              name,
              code,
              Option(rs.getString("DESCR")).getOrElse(""),
              Option(rs.getString("NODE_1")).getOrElse(""),
              Option(rs.getDouble("N1_X")).getOrElse(0.0),
              Option(rs.getDouble("N1_Y")).getOrElse(0.0),
              Option(rs.getDouble("N1_Z")).getOrElse(0.0),
              Option(rs.getString("NODE_2")).getOrElse(""),
              Option(rs.getDouble("N2_X")).getOrElse(0.0),
              Option(rs.getDouble("N2_Y")).getOrElse(0.0),
              Option(rs.getDouble("N2_Z")).getOrElse(0.0),
              Option(rs.getDouble("LENGTH")).getOrElse(0.0),
              materials.find(x => x.code == code) match {
                case Some(value) => value
                case _ => Material()
              }
            )
          }
        }
        catch {
          case e: Exception => println(e.toString)
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res.toList
  }

  def getCablesBySystem(project: String, docNumber: String): List[CableRoute] = {
    val res = ListBuffer.empty[CableRoute];

    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val materials: List[Material] = getMaterials
        val rout = ListBuffer.empty[NodeConnect];

        val s = c.createStatement();

        val queryN = Source.fromResource("queries/elecKeyNodes.sql").mkString
        val rsn = s.executeQuery(queryN)
        try {
          while (rsn.next()) {
            rout += NodeConnect(
              Option(rsn.getInt("SEQID")).getOrElse(0),
              Option(rsn.getString("USERID")).getOrElse(""),
              Option(rsn.getInt("COUNT")).getOrElse(0))
          }
        }
        catch {
          case e: Exception => println(e.toString)
        }

        try {
          val query = Source.fromResource("queries/elecCables.sql").mkString.replaceAll(":docNumber", "'%" + docNumber + "%'");
          val rs = s.executeQuery(query);

          while (rs.next()) {
            val code = Option(rs.getString("STOCK_CODE")).getOrElse("");
            val name = Option(rs.getString("CODE")).getOrElse("");
            val cab_route_area = Option(rs.getString("ROUTE_AREA")).getOrElse("");
            val cab_route_area_id = Option(rs.getString("ROUTE_AREA_ID")).getOrElse("");
            val nodes = ListBuffer.empty[String]
            val nodes_id = ListBuffer.empty[Int]

            if (cab_route_area != "" || cab_route_area_id != "") {
              try {
                val routeArea = cab_route_area.split('^').toList
                val routeAreaId = cab_route_area_id.split(',').toList
                val nodesId = rout.filter(x => routeAreaId.contains(x.id.toString)).sortBy(x => routeAreaId.indexOf(x.id.toString))

                val filterRout = nodesId.filter(_.count > 2)

                nodes_id += nodesId.head.id;

                filterRout.foreach(node => {
                  val i = nodesId.indexOf(node);
                  if (i != 0) {
                    nodes_id += nodesId(i - 1).id;
                  }
                  if (i != nodesId.size - 1) {
                    nodes_id += nodesId(i + 1).id;
                  }
                })

                nodes_id += nodesId.last.id;

                nodes_id.distinct.foreach(node => {
                  val i = routeAreaId.indexOf(node.toString);
                  nodes += routeArea(i);
                })
              }
              catch {
                case e: Exception => println(e.toString)
              }
            }

            res += CableRoute(
              Option(rs.getString("SYSTEM")).getOrElse(""),
              name,
              Option(rs.getString("DESCRIPTION")).getOrElse(""),
              Option(rs.getString("NOM_SECTION")).getOrElse(""),
              Option(rs.getInt("DIAMETER")).getOrElse(0),
              Option(rs.getString("SEG_CODE")).getOrElse(""),
              Option(rs.getString("BUNCH")).getOrElse(""),
              Option(rs.getDouble("F_ROUT")).getOrElse(0),
              Option(rs.getDouble("LENGTH")).getOrElse(0.0),
              Option(rs.getDouble("EXT_LEN_1")).getOrElse(0.0),
              Option(rs.getDouble("EXT_LEN_2")).getOrElse(0.0),
              Option(rs.getString("FROM_SYSTEM")).getOrElse(""),
              Option(rs.getInt("FROM_EQ_ID")).getOrElse(0),
              Option(rs.getString("FROM_EQ_DESC")).getOrElse(""),
              Option(rs.getString("FROM_EQ")).getOrElse(""),
              Option(rs.getString("FROM_STOCK_CODE")).getOrElse(""),
              Option(rs.getDouble("FROM_X")).getOrElse(0.0),
              Option(rs.getDouble("FROM_Y")).getOrElse(0.0),
              Option(rs.getDouble("FROM_Z")).getOrElse(0.0),
              Option(rs.getString("FROM_ZONE")).getOrElse(""),
              Option(rs.getString("FROM_ZONE_DESC")).getOrElse(""),
              Option(rs.getString("TO_SYSTEM")).getOrElse(""),
              Option(rs.getInt("TO_EQ_ID")).getOrElse(0),
              Option(rs.getString("TO_EQ_DESC")).getOrElse(""),
              Option(rs.getString("TO_EQ")).getOrElse(""),
              Option(rs.getString("TO_STOCK_CODE")).getOrElse(""),
              Option(rs.getDouble("TO_X")).getOrElse(0.0),
              Option(rs.getDouble("TO_Y")).getOrElse(0.0),
              Option(rs.getDouble("TO_Z")).getOrElse(0.0),
              Option(rs.getString("TO_ZONE")).getOrElse(""),
              Option(rs.getString("TO_ZONE_DESC")).getOrElse(""),
              nodes.toList,
              nodes_id.distinct.toList,
              code,
              materials.find(x => x.code == code) match {
                case Some(value) => value
                case _ => Material()
              }
            )
          }
          rs.close();
        }
        catch {
          case e: Exception => println(e.toString)
        }
        s.close();
        c.close();
      case _ =>
    }
    res.toList;
  }

  def getEquipmentsBySystem(project: String, docNumber: String): List[EquipmentConnection] = {
    val res = ListBuffer.empty[EquipmentConnection]
    DBManager.GetOracleConnection(project) match {
      case Some(c) => {
        val s = c.createStatement()
        try {
          val query = Source.fromResource("queries/elecEquipments.sql").mkString.replaceAll(":docNumber", "'%" + docNumber + "%'")
          val rs = s.executeQuery(query)
          val allMaterials = retrieveAllMaterialsByProject(project)
          while (rs.next()) {
            val eq = ForanEq(
              Option(rs.getInt("OID")).getOrElse(0),
              Option(rs.getInt("TYPE")).getOrElse(0),
              Option(rs.getString("USERID")).getOrElse(""),
              Option(rs.getInt("ZONE_SEQID")).getOrElse(0),
              Option(rs.getString("ZONE_NAME")).getOrElse(""),
              Option(rs.getString("ZONE_DESCR")).getOrElse(""),
              Option(rs.getInt("SYSTEM_SEQID")).getOrElse(0),
              Option(rs.getString("SYSTEM_NAME")).getOrElse(""),
              Option(rs.getString("SYSTEM_DESCR")).getOrElse(""),
              Option(rs.getString("ABBREV")).getOrElse(""),
              Option(rs.getDouble("WEIGHT")).getOrElse(0.0),
              Option(rs.getString("STOCK_CODE")).getOrElse(""),
              Option(rs.getString("CLASS_NAME")).getOrElse(""),
              Option(rs.getString("RA_CODE")).getOrElse(""),
              Option(rs.getString("RA_DESCR")).getOrElse(""),
              Option(rs.getString("NODE_USERID")).getOrElse(""),
              Option(rs.getString("EQELEC")).getOrElse(""),
              Option(rs.getDouble("XCOG")).getOrElse(0.0),
              Option(rs.getDouble("YCOG")).getOrElse(0.0),
              Option(rs.getDouble("ZCOG")).getOrElse(0.0),
              Option(rs.getDouble("A11")).getOrElse(0.0),
              Option(rs.getDouble("A12")).getOrElse(0.0),
              Option(rs.getDouble("A13")).getOrElse(0.0),
              Option(rs.getDouble("A21")).getOrElse(0.0),
              Option(rs.getDouble("A22")).getOrElse(0.0),
              Option(rs.getDouble("A23")).getOrElse(0.0),
              Option(rs.getDouble("A31")).getOrElse(0.0),
              Option(rs.getDouble("A32")).getOrElse(0.0),
              Option(rs.getDouble("A33")).getOrElse(0.0),
              Option(rs.getDouble("A41")).getOrElse(0.0),
              Option(rs.getDouble("A42")).getOrElse(0.0),
              Option(rs.getDouble("A43")).getOrElse(0.0),
              Option(rs.getDouble("X")).getOrElse(0.0),
              Option(rs.getDouble("Y")).getOrElse(0.0),
              Option(rs.getDouble("Z")).getOrElse(0.0),
              Option(rs.getString("SURFACE")).getOrElse("")
            )

            val resMatrix = DenseMatrix(
              (eq.A11, eq.A21, eq.A31, eq.A41),
              (eq.A12, eq.A22, eq.A32, eq.A42),
              (eq.A13, eq.A23, eq.A33, eq.A43)
            ) *
              DenseMatrix(
                eq.PX, eq.PY, eq.PZ, 1
              )

            val label: String = getEqelecInfo(eq.EQELEC)
            val supports: List[MountItem] = getSupports(eq.EQELEC, allMaterials)
            val material: WorkShopMaterial = findWorkshopMaterialContains(eq.STOCK_CODE, allMaterials)

            res += EquipmentConnection(
              eq.OID,
              label,
              eq.TYPE,
              eq.USERID,
              eq.NODE_USERID,
              eq.ZONE_SEQID,
              eq.ZONE_NAME,
              eq.ZONE_DESCR,
              eq.SYSTEM_SEQID,
              eq.SYSTEM_NAME,
              eq.SYSTEM_DESCR,
              eq.ABBREV,
              eq.XCOG,
              eq.YCOG,
              eq.ZCOG,
              resMatrix.valueAt(0),
              resMatrix.valueAt(1),
              resMatrix.valueAt(2),
              eq.WEIGHT,
              eq.STOCK_CODE,
              eq.CLASS_NAME,
              eq.SURFACE,
              supports,
              material)
          }
          rs.close()
          c.close()
          s.close()
        } catch {
          case e: Exception => println(e.toString)
        }
      }
      case _ =>
    }
    res.toList
  }

  private def getEqelecInfo(in: String): String = {
    if (in.nonEmpty) {
      if (in.contains('\n')) {
        val fr = in.split('\n').head
        if (fr.contains('|')) {
          fr.split('|').head
        } else {
          fr
        }
      } else {
        if (in.contains('|')) {
          in.split('|').head
        } else {
          in
        }
      }
    } else {
      "NF"
    }
  }

  private def getSupports(in: String, wmaterials: List[WorkShopMaterial]): List[MountItem] = {
    val buffer = ListBuffer.empty[MountItem]
    in.split('\n').foreach(row => {
      if (row.count(_ == '|') == 3) {
        val items = row.split('|')
        buffer += MountItem(findWorkshopMaterialContains(items(1), wmaterials), items(0), items(2), items(3).toDoubleOption.getOrElse(0.0), false)
      }
    })
    buffer.toList
  }

  def getElecProjects: List[String] = {
    foranProjects
  }
  def getBlocks(project: String): List[Block] = {
    val blocks = ListBuffer.empty[Block]
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val q = "select * from block"
        val rs = stmt.executeQuery(q)
        while (rs.next()){
          blocks += Block(
            Option(rs.getString("CODE")).getOrElse(""),
            Option(rs.getString("DESCRIPTION")).getOrElse(""),
          )
        }
        rs.close()
        stmt.close()
        connection.close()
      case _ => List.empty[Block]
    }
    blocks.toList
  }
  def getElecZones(project: String): List[Zone] = {
    val zones = ListBuffer.empty[Zone]
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val q = "select zn.name, zl.descr from zone zn, zone_lang zl where zn.oid = zl.zone and zl.lang = -2"
        val rs = stmt.executeQuery(q)
        while (rs.next()) {
          zones += Zone(
            Option(rs.getString("NAME")).getOrElse(""),
            Option(rs.getString("DESCR")).getOrElse(""),
          )
        }
        rs.close()
        stmt.close()
        connection.close()
      case _ => List.empty[Zone]
    }
    zones.toList
  }
  def getElecSystems(project: String): List[System] = {
    val systems = ListBuffer.empty[System]
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val q = "select st.name, sl.descr from systems st, systems_lang sl where sl.system = st.oid and sl.lang = -2"
        val rs = stmt.executeQuery(q)
        while (rs.next()) {
          systems += System(
            Option(rs.getString("NAME")).getOrElse(""),
            Option(rs.getString("DESCR")).getOrElse(""),
          )
        }
        rs.close()
        stmt.close()
        connection.close()
      case _ => List.empty[System]
    }
    systems.toList
  }
  def getEleComplects(project: String): List[EleComplect] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollection: MongoCollection[EleComplect] = mongo.getCollection("eleComplects")
        Await.result(espCollection.find(equal("project", project)).toFuture(), Duration(10, SECONDS)) match {
          case complects: List[EleComplect] => complects
          case _ => List.empty[EleComplect]
        }
      case _ => List.empty[EleComplect]
    }
  }
  def addEleComplect(json: String): Unit = {
    decode[EleComplect](json) match {
      case Right(value) =>
        DBManager.GetMongoConnection() match {
          case Some(mongo) =>
            val espCollection: MongoCollection[EleComplect] = mongo.getCollection("eleComplects")
            Await.result(espCollection.insertOne(value).toFuture(), Duration(10, SECONDS))
          case _ => None
        }
      case Left(value) =>
    }
  }
  def updateEleComplect(json: String): Unit = {
    decode[EleComplect](json) match {
      case Right(value) =>
        DBManager.GetMongoConnection() match {
          case Some(mongo) =>
            val espCollection: MongoCollection[EleComplect] = mongo.getCollection("eleComplects")
            Await.result(espCollection.replaceOne(equal("drawingId", value.drawingId), value).toFuture(), Duration(10, SECONDS))
          case _ => None
        }
      case Left(value) =>
    }
  }

  def updatePosEleEsp(
                       code: String,
                       count: Double,
                       docNumber: String,
                       prevPos: String,
                       newPos: String
                     ): String = {
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val selectQuery = "SELECT 1 FROM issue_materials WHERE material_stock_code = ? AND count = ? AND doc_number = ? AND pos = ?"
        val updateQuery = "UPDATE issue_materials SET pos = ? WHERE material_stock_code = ? AND count = ? AND doc_number = ? AND pos = ?"

        val selectStmt = c.prepareStatement(selectQuery)
        selectStmt.setString(1, code)
        selectStmt.setDouble(2, count)
        selectStmt.setString(3, docNumber)
        selectStmt.setString(4, prevPos)
        val rs = selectStmt.executeQuery()

        if (rs.next()) {
          val updateStmt = c.prepareStatement(updateQuery)
          updateStmt.setString(1, newPos)
          updateStmt.setString(2, code)
          updateStmt.setDouble(3, count)
          updateStmt.setString(4, docNumber)
          updateStmt.setString(5, prevPos)
          updateStmt.executeUpdate()
          updateStmt.close()
        }
        rs.close()
        selectStmt.close()
        c.close()
      case _ => None
    }
    "success"
  }


//  def updatePosEleEsp(
//                 code: String,       // material_stock_code в БД
//                 count: Int,
//                 docNumber: String,
//                 prevPos: String,       // предыдущее значение pos
//                 newPos: String        // новое значение pos
//               ): String = {
//    DBManager.GetPGConnection() match {
//      case Some(c) =>
//        val s = c.createStatement()
//        // Проверим, есть ли такая строка по четырём полям
//        val rs = s.executeQuery(
//          s"select * from issue_materials where material_stock_code = '$code' and count = $count and doc_number = '$docNumber' and pos = '$prevPos'"
//        )
//        println(rs)
//
//        val entries = ListBuffer.empty[Int]
//        while (rs.next()) {
//          entries += rs.getInt("pos")
//        }
//
//        if (entries.nonEmpty) {
//          // Обновляем pos только в найденной строке
//          println("updateElePos")
//          s.execute(
//            s"update issue_materials set pos = '$newPos' where material_stock_code = '$code' and count = $count and doc_number = '$docNumber' and pos = '$prevPos'"
//          )
//        }
//        rs.close()
//        s.close()
//        c.close()
//      case _ => None
//    }
//    "success"
//  }

  def deleteEleComplect(drawing: String): Unit = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollection: MongoCollection[EleComplect] = mongo.getCollection("eleComplects")
        Await.result(espCollection.deleteOne(equal("drawingId", drawing)).toFuture(), Duration(10, SECONDS))
      case _ => None
    }
  }

  def getMaterialsLabels: List[MaterialLabel] = {
    val res = ListBuffer.empty[MaterialLabel]
    DBManager.GetPGConnection() match {
      case Some(c) =>
        val s = c.createStatement()
        val query = "select stock_code, default_label from materials where default_label is not null"
        try {
          val rs = s.executeQuery(query)
          while (rs.next()) {
            res += MaterialLabel(
              rs.getString("stock_code"),
              rs.getString("default_label"),
            )
          }
          rs.close()
          s.close()
          c.close()
        }
        catch {
          case e: Exception =>
            s.close()
            c.close()
        }
      case _ =>
    }
    res.toList
  }

  def getEleTrays(project: String, systems: List[String], materials: List[Material]): List[EleTray] = {
    val res = ListBuffer.empty[EleTray]

    val materialsLabels = getMaterialsLabels

    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        try{

          val systemsList = systems.map(x => '\'' + x + '\'').mkString(",")

          val plsParams = ListBuffer.empty[PlsParam]
          val rsParams = stmt.executeQuery(s"SELECT * FROM PLSE_PAROBJ_REALPAR WHERE SYSTEM IN (SELECT SEQID FROM SYSTEMS WHERE NAME IN ($systemsList))")
          while (rsParams.next()){
            plsParams += PlsParam(
              Pls(Option(rsParams.getInt("TYPE")).getOrElse(0),
                Option(rsParams.getInt("ZONE")).getOrElse(0),
                Option(rsParams.getInt("SYSTEM")).getOrElse(0),
                Option(rsParams.getString("LINE")).getOrElse(""),
                Option(rsParams.getInt("PLS")).getOrElse(0),
                Option(rsParams.getInt("ELEM")).getOrElse(0)),
              Option(rsParams.getInt("PARAM_OBJ")).getOrElse(0),
              Option(rsParams.getInt("PARAM_SQ")).getOrElse(0),
              Option(rsParams.getDouble("VALUE")).getOrElse(0),
            )
          }

          val query = Source.fromResource("queries/eleTrays.sql").mkString.replaceAll("&systemList", systemsList)
          val rs = stmt.executeQuery(query)
          while (rs.next()){
            val userId = Option(rs.getString("USERID")).getOrElse("")
            val trayFit = Option(rs.getInt("TRAY_FITTING")).getOrElse(0)
            val kind = if (trayFit == 0) "TRAY" else "TRANSIT"
            val stock = if (kind == "TRAY") {
              Option(rs.getString("STOCK_CODE")).getOrElse("")
            } else {
              Option(rs.getString("COMP_STOCK")).getOrElse("")
            }
            val pos = materialsLabels.find(_.code == stock) match {
              case Some(value) => value.label
              case _ => userId
            }
            val cType = rs.getString("CTYPE")
            val pls = Pls(Option(rs.getInt("TYPE")).getOrElse(0),
              Option(rs.getInt("ZONE")).getOrElse(0),
              Option(rs.getInt("SYSTEM")).getOrElse(0),
              Option(rs.getString("LINE")).getOrElse(""),
              Option(rs.getInt("PLS")).getOrElse(0),
              Option(rs.getInt("ELEM")).getOrElse(0))

            val params = plsParams.filter(_.pls.equals(pls))

            if (stock == "ESNCARCTSXXX0001"){
              val q = 0
            }
            val material = materials.find(_.code == stock) match {
              case Some(value) => value
              case _ => Material().copy(name = "No stock code, userId " + userId, units = "006")
            }

            val length = {
              material.units match {
                case "796" => 1
                case _ =>
                  cType match {
                    //case "A" => material.singleWeight
                    case "B" =>
                      if (params.length == 4) {
                        val angle = params(3).value * 180 / Math.PI
                        Math.PI * params(2).value * angle / 180 / 1000d
                      }
                      else{
                        0
                      }
                    case "P" =>
                      if (params.nonEmpty) params.last.value / 1000d else 0
                    case _ => 0
                  }
              }
            }

            if (length < 0.015){
              val q = 0
            }

            res += EleTray(
              pos,
              stock,
              length,
              cType,
              rs.getInt("IDSQ"),
              rs.getInt("NODE1"),
              rs.getInt("NODE2"),
              kind,
              Cog(
                rs.getDouble("X_COG"),
                rs.getDouble("Y_COG"),
                rs.getDouble("Z_COG"),
              ),
              Option(rs.getString("zone_name")).getOrElse(""),
              material.units,
              material
            )
          }
          rs.close()
          stmt.close()
          connection.close()
        }
        catch {
          case e: Exception =>
            stmt.close()
            connection.close()
        }
      case _ =>
    }
    res.toList
  }
  def getEleEquips(project: String, zones: List[String], materials: List[Material]): List[EleEquip] = {
    val res = ListBuffer.empty[EleEquip]
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        try {
          val query = Source.fromResource("queries/eleEquips.sql").mkString.replaceAll("&zoneList", zones.map(x => '\'' + x + '\'').mkString(","))
          val rs = stmt.executeQuery(query)
          while (rs.next()) {
            val stock = Option(rs.getString("STOCK_CODE")).getOrElse("")
            val userId = Option(rs.getString("USERID")).getOrElse("")
            val abbrev = Option(rs.getString("ABBREV")).getOrElse("")
            val wgt = rs.getDouble("WEIGHT")
            val material = materials.find(_.code == stock) match {
              case Some(value) => value
              case _ => Material().copy(name = "No stock code, " + abbrev)
            }
            res += EleEquip(
              userId,
              stock,
              abbrev,
              if (wgt == 0) material.singleWeight else wgt,
              Cog(
                (rs.getDouble("XMIN") + rs.getDouble("XMAX")) / 2d,
                (rs.getDouble("YMIN") + rs.getDouble("YMAX")) / 2d,
                (rs.getDouble("ZMIN") + rs.getDouble("ZMAX")) / 2d
              ),
              Option(rs.getString(38)).getOrElse(""),
              material
            )
          }
          rs.close()
          stmt.close()
          connection.close()
        }
        catch {
          case e: Exception =>
            stmt.close()
            connection.close()
        }
      case _ =>
    }
    res.toList
  }
  def getEleEsp(foranProject: String, systems: List[String], zones: List[String], materials: List[Material], issueId: Int): List[EleElement] = {
    val trays = getEleTrays(foranProject, systems, materials).map(x => EleElement(x.userId, x.kind, x.units, x.weight, x.stock, x.material, x.cog, x.zone, 1))
    val equips = getEleEquips(foranProject, zones, materials).map(x => EleElement(x.userId, "EQUIP", "796", x.weight, x.stock, x.material, x.cog, x.zone, 1))
    val manual = getEleIssueMaterials(issueId).map(x => {
      val material = materials.find(_.code == x.stock).getOrElse(Material())
      if (material.code == "ESNCARCTSXXX0001"){
        val q = 0
      }
      val weight = x.units match {
        case "006" => x.count * material.singleWeight
        case "796" => x.count * material.singleWeight
        case "166" => x.count
        case _ => x.weight
      }
      EleElement(x.pos, "MANUAL", x.units, weight, x.stock, material, Cog(0, 0, 0), x.zone, x.count)
    })
    trays ++ equips ++ manual
  }
  def getEleIssueMaterials(issueId: Int): List[IssueMaterial] = {
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
            rs.getString("department"),
            rs.getString("zone"),
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ => None
    }
    res.toList
  }
  def generateEleEsp(foranProject: String, docNumber: String, rev: String, user: String, taskId: String): EleEspObject = {
    val id = UUID.randomUUID().toString
    val date = new Date().getTime
    val projects = getIssueProjects
    val rkdProject = projects.find(_.foran == foranProject) match {
      case Some(value) => value.rkd
      case _ => ""
    }
    val materials = getMaterials.filter(_.project == rkdProject)
    val complects = getEleComplects(foranProject)
    val complect = complects.find(_.drawingId == docNumber) match {
      case Some(value) => value
      case _ => EleComplect(docNumber, "", "", foranProject, List.empty[String], List.empty[String])
    }
    EleEspObject(id, foranProject, docNumber, rev, date, user, "ele", taskId.toIntOption.getOrElse(0), elements = getEleEsp(foranProject, complect.systemNames, complect.zoneNames, materials, taskId.toIntOption.getOrElse(0)))
  }
  def getIssueName(id: Int): String = {
    val res = ListBuffer.empty[String]
    DBManager.GetPGConnection() match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val rs = stmt.executeQuery(s"select issue_name from issue where id = $id")
        while (rs.next()){
          res += rs.getString("issue_name")
        }
        stmt.close()
        connection.close()
      case _ => None
    }
    res.headOption.getOrElse("")
  }
  def getElePos(project: String, kind: String, index: Int, taskId: Int): ElePos = {
    kind match {
      case "tray" => getTray(project, index, kind).getOrElse(ElePos(project, kind, "NO STOCK", "TRAY NOT FOUND", "", ""))
      case "transit" => getTransit(project, index, kind).getOrElse(ElePos(project, kind, "NO STOCK", "TRANSIT NOT FOUND", "", ""))
      case "equip" => getEquip(project, index, kind, taskId).getOrElse(ElePos(project, kind, "NO STOCK", "EQUIP NOT FOUND", "", ""))
      case _ => ElePos(project, kind, "NO STOCK", "UNKNOWN LABEL TYPE", "", "")
    }
  }
  private def getTray(project: String, index: Int, kind: String): Option[ElePos] = {
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val query = "select * from pls_elem pls\nleft join v_cabletray_stockcode vcs on vcs.system = pls.system and vcs.zone = pls.zone and vcs.line = pls.line and vcs.pls = pls.pls and vcs.elem = pls.elem\nwhere pls.idsq = " + index.toString
        val rs = stmt.executeQuery(query)
        val codes = Iterator.continually(rs.next()).takeWhile(identity).map(_ => rs.getString("STOCK_CODE")).toList
        rs.close()
        stmt.close()
        connection.close()
        val stock = codes.headOption.getOrElse("")
        val label = if (stock == "") "NO STOCK CODE" else getSpecMaterial(stock).headOption match {
          case Some(material) => material.label
          case _ => "MATERIAL NOT FOUND FOR STOCK CODE " + stock
        }
        Option(ElePos(project, kind, stock, label, "", ""))
      case _ => Option.empty[ElePos]
    }
  }
  private def getTransit(project: String, index: Int, kind: String): Option[ElePos] = {
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val query = "select * from pls_elem pls\nleft join v_transit tr on tr.oid = pls.tray_fitting and tr.zone = pls.ZONE and tr.syst = pls.system\nleft join v_node_type_conn nd on nd.SEQID = pls.node1\nwhere pls.idsq = " + index.toString
        val rs = stmt.executeQuery(query)
        val elePos = Iterator.continually(rs.next()).takeWhile(identity).map(_ => {
          val stock = rs.getString("COMP_STOCK")
          val descr = rs.getString("COMP_DESCR")
          val node = rs.getString("CODE")
          val label = if (stock == "") "NO STOCK CODE" else getSpecMaterial(stock).headOption match {
            case Some(material) => material.label
            case _ => "MATERIAL NOT FOUND FOR STOCK CODE " + stock
          }
          ElePos(project, kind, stock, label, node, descr)
        }).toList
        rs.close()
        stmt.close()
        connection.close()
        elePos.headOption
      case _ => Option.empty[ElePos]
    }
  }
  private def getEquip(project: String, oid: Int, kind: String, taskId: Int): Option[ElePos] = {
    val issueMaterials = getEleIssueMaterials(taskId)
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val query = "select userid from element where oid = " + oid.toString
        val rs = stmt.executeQuery(query)
        val elePos = Iterator.continually(rs.next()).takeWhile(identity).map(_ => {
          val userId = rs.getString("userid")
          val materials = issueMaterials.filter(_.pos.contains(userId)).sortBy(_.pos.length).reverse
          val materialLabel = if (materials.nonEmpty){
            getSpecMaterial(materials.head.stock).headOption match {
              case Some(material) => material.label
              case _ => "MATERIAL NOT FOUND"
            }
          }
          else{
            "NO MATERIAL"
          }
          ElePos(project, kind, "", userId, materialLabel, "")
        }).toList
        rs.close()
        stmt.close()
        connection.close()
        elePos.headOption
      case _ => Option.empty[ElePos]
    }
  }


  def getEleNodes(project: String, node_oid: Int = 0): List[EleNode] = {
    val res = ListBuffer.empty[EleNode]
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val query = "select * from v_node_penetration np, v_node n where np.NODE = n.NODE and np.type = 2" + (if (node_oid != 0) s" and NODE_OID = $node_oid" else "")
        val rs = stmt.executeQuery(query)
        while (rs.next()){
          try{
            res += EleNode(
              Option(rs.getInt("NODE_OID")).getOrElse(0),
              Option(rs.getString("NODE")).getOrElse(""),
              Option(rs.getDouble("X")).getOrElse(0d),
              Option(rs.getDouble("Y")).getOrElse(0d),
              Option(rs.getDouble("Z")).getOrElse(0d),
              Option(rs.getInt("TYPE")).getOrElse(0),
              Option(rs.getInt("AREA_ID")).getOrElse(0),
              Option(rs.getString("ROUT_AREA")).getOrElse(""),
              Option(rs.getString("AREA_DESCR")).getOrElse(""),
              Option(rs.getString("CODE")).getOrElse(""),
              Option(rs.getString("DESCR")).getOrElse(""),
              Option(rs.getDouble("FRAMES")).getOrElse(0d),
              Option(rs.getDouble("IWIDTH")).getOrElse(0d),
              Option(rs.getDouble("IHEIGHT")).getOrElse(0d),
              Option(rs.getDouble("LENGTH")).getOrElse(0d),
              Option(rs.getDouble("THICKNESS")).getOrElse(0d),
              Option(rs.getDouble("HEIGHT2")).getOrElse(0d),
              Option(rs.getDouble("WEIGHT")).getOrElse(0d),
              Option(rs.getString("STOCK_CODE")).getOrElse(""),
              Option(rs.getDouble("NROWS")).getOrElse(0d),
              Option(rs.getDouble("NCOLUMNS")).getOrElse(0d),
              Option(rs.getString("SEAL_TYPE")).getOrElse(""),
              Option(rs.getString("TRANSIT_SIZE")).getOrElse(""),
              "NO"
            )
          }
         catch {
           case e: Throwable => println(e.toString)
         }
        }
        rs.close()
        stmt.close()
        connection.close()
        res.toList
      case _ => List.empty[EleNode]
    }
  }
  def getEleNodesError(project: String, node_id: Int = 0): List[EleNode] = {
    try{
      val nodes = getEleNodes(project, node_id)
      val nodeModules = getEleNodeModules(project)
      val materials = getMaterials
      nodes.map(node => node.copy(error = {
        val png = createNodeModulesPNG(project, node.node_id, materials, nodeModules, deleteFile = true)
        if (png.png_url.contains("error")) png.png_url else "ok"
      }))
    }
    catch {
      case e: Throwable => List.empty[EleNode]
    }
  }
  def getEleNodeCables(project: String, node: Int): List[EleCable] = {
    try{
      val res = ListBuffer.empty[EleCable]
      DBManager.GetOracleConnection(project) match {
        case Some(connection) =>
          val stmt = connection.createStatement()
          val query = s"select * from v_cable where seqid in (select cable_sid from v_node_cables where node_sid = $node)"
          val rs = stmt.executeQuery(query)
          while (rs.next()){
            try{
              res += EleCable(
                Option(rs.getString("CABLE_ID")).getOrElse(""),
                Option(rs.getString("NOM_SECTION")).getOrElse(""),
                Option(rs.getString("CABLE_SPEC")).getOrElse(""),
                Option(rs.getString("MAT_NUMBER")).getOrElse(""),
                Option(rs.getDouble("O_DIAMETER")).getOrElse(0d),
              )
            }
            catch {
              case e: Throwable => println(e.toString)
            }
          }
          rs.close()
          stmt.close()
          connection.close()
          res.toList
        case _ => List.empty[EleCable]
      }
    }
    catch {
      case e: Throwable => List.empty[EleCable]
    }
  }
  def getEleNodeModules(project: String): List[EleNodeModule] = {
    val res = ListBuffer.empty[EleNodeModule]
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val query = "select * from cab_block"
        val rs = stmt.executeQuery(query)
        while (rs.next()){
          try{
            res += EleNodeModule(
              Option(rs.getString("CODE")).getOrElse(""),
              Option(rs.getString("BLOCK_TYPE")).getOrElse(""),
              Option(rs.getDouble("O_WIDTH")).getOrElse(0d),
              Option(rs.getDouble("I_DIAM")).getOrElse(0d),
              Option(rs.getDouble("MX_DIAM")).getOrElse(0d),
            )
          }
          catch {
            case e: Throwable => println(e.toString)
          }
        }
        rs.close()
        stmt.close()
        connection.close()
        res.toList
      case _ => List.empty[EleNodeModule]
    }
  }
  def createNodeModulesPNG(project: String, node_id: Int, materialsSrc: List[Material] = List.empty[Material], nodeModulesSrc: List[EleNodeModule] = List.empty[EleNodeModule], scale: Int = 4, numeric: Boolean = false, step: Int = 1, deleteFile: Boolean = false): EleNodePNG = {
    try{

      val materials = if (materialsSrc.isEmpty) getMaterials else materialsSrc
      val nodeModules = if (nodeModulesSrc.isEmpty) getEleNodeModules(project) else nodeModulesSrc

      val specText = ListBuffer.empty[String]
      val spec = ListBuffer.empty[EleNodeSpec]
      val specCables = ListBuffer.empty[EleCableSpec]

      val nodes = getEleNodes(project, node_id)
      val cables = getEleNodeCables(project, node_id)
      var error = ""
      var totalRows = 0
      val modules = ListBuffer.empty[Int]
      val fillerModules = ListBuffer.empty[Int]
      val fillerNoModules = ListBuffer.empty[Int]


      nodes.find(_.node_id == node_id) match {
        case Some(node) =>

          val p = 1
          val colP = 5

          val split = node.descr.split("\n").lastOption.getOrElse("").replace("х", "x")
          val wh = split.split("x")
          val nodeWidth = wh(0).toIntOption.getOrElse(0)
          val nodeHeight = wh(1).toIntOption.getOrElse(0)

          if (nodeHeight == 0 || nodeWidth == 0) {
            error = "error: node height or width is zero"
          }


          var pic = Image.rectangle(nodeWidth * node.ncolumns + p, nodeHeight * node.nrows + p).strokeWidth(0)

          val xStart = -1 * nodeWidth * node.ncolumns / 2d - p
          val yStart = nodeHeight * node.nrows / 2d + p


          (0.until(node.nrows.toInt)).foreach(row => {
            (0.until(node.ncolumns.toInt)).foreach(col => {
              pic = Image.rectangle(nodeWidth + p, nodeHeight + p)
                .strokeWidth(0.5)
                .at(xStart + col * nodeWidth + nodeWidth / 2d + p + colP * col, yStart - row * nodeHeight - nodeHeight / 2d - p).on(pic)
              //pic = Image.rectangle(nodeWidth, node.iheight).fillColor(Color.lightGray).strokeWidth(0.5).at(xStart + col * nodeWidth + nodeWidth / 2d, yStart - row * node.iheight - node.iheight / 2d).on(pic)
            })
          })


          var x = xStart
          var y = yStart
          var col = 0
          var row = 0

          val colDiams = ListBuffer.empty[Double]
          val rowDiams = ListBuffer.empty[Double]

          val usedInsert = ListBuffer.empty[String]


          val cablesSort = cables.sortBy(x => (x.diam, x.diamModule(nodeModules, step, modules.toList), x.cable_id)).reverse
          cablesSort.foreach(cab => {
            if (!usedInsert.contains(cab.cable_id)){
              val diam = cab.diamModule(nodeModules, step, modules.toList)
              modules += diam
              val moduleName = materials.find(_.name.contains("Уплотнительный модуль МКС " + diam.toString)) match {
                case Some(value) => value.name.replace("Уплотнительный модуль ", "")
                case _ => "Не найден"
              }
              if (diam > 0){
                if (colDiams.nonEmpty && colDiams.lastOption.getOrElse(0d) != diam){
                  if (colDiams.sum + colDiams.lastOption.getOrElse(0d) <= nodeWidth){
                    val fillDiam = colDiams.lastOption.getOrElse(0d)
                    val fillCount = nodeWidth / fillDiam - colDiams.length
                    (0.until(fillCount.toInt)).foreach(filler => {
                      x = xStart + col * nodeWidth + colDiams.length * fillDiam + filler * fillDiam

                      if (col != 0) {
                        val r = Image.rectangle(fillDiam - p, fillDiam - p).strokeColor(Color.red)
                          .strokeWidth(0.5).fillColor(Color.white).at(x + Math.ceil(fillDiam / 1.5d) + 2 * p, y - fillDiam / 2 - p)  //на первой и второй страницах
                        pic = r.on(pic)
                        val t = Image.text(
                          if (numeric) (specCables.length + 1).toString else fillDiam.toInt.toString
                        ).scale(fillDiam * 1.5 / nodeWidth, fillDiam * 1.5 / nodeWidth).at(x +  Math.ceil(fillDiam / 1.5d) + 2*p, y - fillDiam / 2 - p)
                        pic = t.on(pic)
                      }
                      else {
                        val r = Image.rectangle(fillDiam - p, fillDiam - p).strokeColor(Color.red)
                          .strokeWidth(0.5).fillColor(Color.white).at(x +  Math.ceil(fillDiam / 2d) + p, y - fillDiam / 2 - p)  //на первой и второй страницах
                        pic = r.on(pic)
                        val t = Image.text(
                          if (numeric) (specCables.length + 1).toString else fillDiam.toInt.toString
                        ).scale(fillDiam * 1.5 / nodeWidth, fillDiam * 1.5 / nodeWidth).at(x +  Math.ceil(fillDiam / 2d) + p, y - fillDiam / 2 - p)
                        pic = t.on(pic)
                      }
//                      val r = Image.rectangle(fillDiam - p, fillDiam - p).strokeColor(Color.red)
//                        .strokeWidth(0.5).fillColor(Color.white).at(x +  Math.ceil(fillDiam / 2d) + p, y - fillDiam / 2 - p)  //на первой и второй страницах
//                      pic = r.on(pic)
//                      val t = Image.text(
//                        if (numeric) (specCables.length + 1).toString else fillDiam.toInt.toString
//                      ).scale(fillDiam * 1.5 / nodeWidth, fillDiam * 1.5 / nodeWidth).at(x +  Math.ceil(fillDiam / 2d) + p, y - fillDiam / 2 - p)
//                      pic = r.on(pic)
//                      pic = t.on(pic)
                      fillerModules += fillDiam.toInt

                      val moduleName = materials.find(_.name.contains("Уплотнительный модуль МКС " + fillDiam.toInt.toString)) match {
                        case Some(value) => value.name.replace("Уплотнительный модуль ", "")
                        case _ => "Не найден"
                      }

                      specCables += EleCableSpec((specCables.length + 1).toString, "Модуль", moduleName)

                    })
                  }
                  y += -1 * colDiams.lastOption.getOrElse(0d)
                  rowDiams += colDiams.lastOption.getOrElse(0d)
                  colDiams.clear()
                  totalRows += 1
                }
                else if (colDiams.nonEmpty && (colDiams.sum + diam) > nodeWidth){
                  if (step != 1){
                    val insertDiam = nodeWidth - colDiams.sum
                    val filterInsert = cablesSort.filter(x => x.diamModule(nodeModules, step, modules.toList) == insertDiam)
                    if (filterInsert.nonEmpty){
                      0.until(diam / insertDiam.toInt).foreach(insertCableIndex => {
                        val insertCables = filterInsert.find(x => !usedInsert.contains(x.cable_id)).take(1)
                        if (insertCables.nonEmpty){
                          val insertCable = insertCables.head
                          usedInsert += insertCable.cable_id
                          x = xStart + col * nodeWidth + col * colP + diam
                          val r = Image.rectangle(insertDiam - p, insertDiam - p)
                            .strokeColor(Color.blue).strokeWidth(0.5).fillColor(Color.white)
                            .at(x + Math.ceil(insertDiam / 2d) + p, y - insertDiam / 2 - p - insertCableIndex * insertDiam)
                          val t = Image.text(if (numeric) (specCables.length + 1).toString else insertCable.cable_id)
                            .scale(insertDiam * 1.5 / 120, insertDiam * 1.5 / 120)
                            .at(x +  Math.ceil(insertDiam / 2d) + p, y - insertDiam / 2 - p - insertCableIndex * insertDiam)
                          pic = r.on(pic)
                          pic = t.on(pic)
                          modules += insertDiam.toInt

                          specCables += EleCableSpec((specCables.length + 1).toString, insertCable.cable_id, moduleName)
                        }
                        else{
                          x = xStart + col * nodeWidth + col * colP + diam
                          val r = Image.rectangle(insertDiam - p, insertDiam - p)
                            .strokeColor(Color.red).strokeWidth(0.5).fillColor(Color.white)
                            .at(x + Math.ceil(insertDiam / 2d) + p, y - insertDiam / 2 - p - insertCableIndex * insertDiam)
                          val t = Image.text(if (numeric) (specCables.length + 1).toString else insertDiam.toInt.toString)
                            .scale(insertDiam * 1.5 / nodeWidth, insertDiam * 1.5 / nodeWidth)
                            .at(x +  Math.ceil(insertDiam / 2d) + p, y - insertDiam / 2 - p - insertCableIndex * insertDiam)
                          pic = r.on(pic)
                          pic = t.on(pic)
                          fillerModules += insertDiam.toInt

                          val moduleName = materials.find(_.name.contains("Уплотнительный модуль МКС " + diam.toString)) match {
                            case Some(value) => value.name.replace("Уплотнительный модуль ", "")
                            case _ => "Не найден"
                          }
                          specCables += EleCableSpec((specCables.length + 1).toString, "Модуль", moduleName)
                        }
                      })
                    }
                    y += -1 * diam
                    rowDiams += diam
                    colDiams.clear()
                    totalRows += 1
                  }
                  else{
                    y += -1 * colDiams.lastOption.getOrElse(0d)
                    rowDiams += colDiams.lastOption.getOrElse(0d)
                    colDiams.clear()
                    totalRows += 1
                  }
                }
                if (rowDiams.sum + diam > nodeHeight){
                  if (col < node.ncolumns - 1){
                    while (error == "" && rowDiams.sum + 5 <= nodeHeight){  //заполнение красных в нехватающую высоту после синих
                      val diam = if (rowDiams.sum + 20 <= nodeHeight){
                        20
                      }
                      else if (rowDiams.sum + 15 <= nodeHeight){
                        15
                      }
                      else if (rowDiams.sum + 10 <= nodeHeight){
                        10
                      }
                      else{
                        5
                      }
                      x = xStart + col * nodeWidth + col * colP

                      if (diam <= 10){
//                        val r = Image.rectangle(nodeWidth - p, diam - p)
//                          .strokeColor(Color.red).strokeWidth(0.5).fillColor(Color.white).at(x + nodeWidth / 2 + p, rowDiams.sum + diam)  //выровнить эту штуку
                          val r = Image.rectangle(nodeWidth - p, diam - p)
                            .strokeColor(Color.red).strokeWidth(0.5).fillColor(Color.white).at(x + nodeWidth / 2 + p,  -1 * rowDiams.sum/2)
                        val t = Image.text(if (numeric) (specCables.length + 1).toString else diam.toString).scale(diam * 4.5 / nodeWidth, diam * 4.5 / nodeWidth).at(x + nodeWidth / 2 + p, y - diam / 2 - p)
                        pic = r.on(pic)
                        pic = t.on(pic)
                        fillerNoModules += diam

                        val moduleName = materials.find(x => x.name.contains("Глухой модуль МКС " + diam.toString) && x.name.contains(nodeWidth.toString)) match {
                          case Some(value) => value.name.replace("Глухой модуль ", "")
                          case _ => "Не найден"
                        }
                        specCables += EleCableSpec((specCables.length + 1).toString, "Модуль", moduleName)
                      }
                      else{
                        val fillCount = nodeWidth / diam
                        (0.until(fillCount)).foreach(filler => {
                          x = xStart + col * nodeWidth + col * colP + filler * diam
                          val r = Image.rectangle(diam - p, diam - p)
                            .strokeColor(Color.red).strokeWidth(0.5).fillColor(Color.white).at(x + Math.ceil(diam / 2d) + p, y - diam / 2 - p)
                          val t = Image.text(if (numeric) (specCables.length + 1).toString else diam.toString)
                            .scale(diam * 1.5 / nodeWidth, diam * 1.5 / nodeWidth).at(x +  Math.ceil(diam / 2d) + p, y - diam / 2 - p)
                          pic = r.on(pic)
                          pic = t.on(pic)

                          val moduleName = materials.find(_.name.contains("Уплотнительный модуль МКС " + diam.toString + "/")) match {
                            case Some(value) => value.name.replace("Уплотнительный модуль ", "")
                            case _ => "Не найден"
                          }
                          fillerModules += diam

                          specCables += EleCableSpec((specCables.length + 1).toString, "Модуль", moduleName)
                        })

                      }
                      totalRows += 1
                      rowDiams += diam
                    }
//
//
//
                    col += 1
                  }
                  else if (row < node.nrows - 1){
                    row += 1
                    col = 0
                  }
                  else{
                    col += 1
                    println("error: not enough")
                    error = "error: not enough space for cables, placed " + cablesSort.indexOf(cab).toString + " of " + cablesSort.length.toString
                  }
                  y = yStart - row * nodeHeight
                  rowDiams.clear()
                  colDiams.clear()
                }

                if (colDiams.isEmpty){
                  x = xStart + col * nodeWidth + col * colP
                }
                else{
                  x = xStart + col * nodeWidth + col * colP + colDiams.length * diam
                }

                val r = Image.rectangle(diam - p, diam - p)
                  .strokeColor(Color.blue).strokeWidth(0.5).fillColor(Color.white).at(x + Math.ceil(diam / 2d) + p, y - diam / 2 - p)
                val t = Image.text(if (numeric) (specCables.length + 1).toString else cab.cable_id)
                  .scale(diam * 1.5 / 120, diam * 1.5 / 120).at(x +  Math.ceil(diam / 2d) + p, y - diam / 2 - p)
                pic = r.on(pic)
                pic = t.on(pic)

                colDiams += diam
                specCables += EleCableSpec((specCables.length + 1).toString, cab.cable_id, moduleName)


                if (cablesSort.indexOf(cab) == cablesSort.length - 1 && colDiams.nonEmpty && colDiams.sum + diam <= nodeWidth){
                  val fillCount = nodeWidth / diam - colDiams.length
                  (0.until(fillCount)).foreach(filler => {
                    x = xStart + col * nodeWidth + col * colP + colDiams.length * diam + filler * diam
                    val r = Image.rectangle(diam - p, diam - p)
                      .strokeColor(Color.red).strokeWidth(0.5).fillColor(Color.white).at(x + Math.ceil(diam / 2d) + p, y - diam / 2 - p)
                    val t = Image.text(if (numeric) (specCables.length + 1).toString else diam.toString)
                      .scale(diam * 1.5 / nodeWidth, diam * 1.5 / nodeWidth).at(x +  Math.ceil(diam / 2d) + p, y - diam / 2 - p)
                    pic = r.on(pic)
                    pic = t.on(pic)
                    fillerModules += diam

                    val moduleName = materials.find(_.name.contains("Уплотнительный модуль МКС " + diam.toString)) match {
                      case Some(value) => value.name.replace("Уплотнительный модуль ", "")
                      case _ => "Не найден"
                    }
                    specCables += EleCableSpec((specCables.length + 1).toString, "Модуль", moduleName)
                  })
                }
                else if (!cablesSort.filter(x => cablesSort.indexOf(x) > cablesSort.indexOf(cab)).exists(x => !usedInsert.contains(x.cable_id)) && colDiams.nonEmpty && step != 1){
                  val insertDiam = nodeWidth - colDiams.sum
                  val filterInsert = cablesSort.filter(x => x.diamModule(nodeModules, step, modules.toList) == insertDiam)
                  if (filterInsert.nonEmpty){
                    0.until(diam / insertDiam.toInt).foreach(insertCableIndex => {
                      x = xStart + col * nodeWidth + col * colP + diam
                      val r = Image.rectangle(insertDiam - p, insertDiam - p)
                        .strokeColor(Color.red).strokeWidth(0.5).fillColor(Color.white)
                        .at(x + Math.ceil(insertDiam / 2d) + p, y - insertDiam / 2 - p - insertCableIndex * insertDiam)
                      val t = Image.text(if (numeric) (specCables.length + 1).toString else insertDiam.toInt.toString)
                        .scale(insertDiam * 1.5 / nodeWidth, insertDiam * 1.5 / nodeWidth)
                        .at(x +  Math.ceil(insertDiam / 2d) + p, y - insertDiam / 2 - p - insertCableIndex * insertDiam)
                      pic = r.on(pic)
                      pic = t.on(pic)
                      fillerModules += insertDiam.toInt

                      val moduleName = materials.find(_.name.contains("Уплотнительный модуль МКС " + diam.toString)) match {
                        case Some(value) => value.name.replace("Уплотнительный модуль ", "")
                        case _ => "Не найден"
                      }
                      specCables += EleCableSpec((specCables.length + 1).toString, "Модуль", moduleName)
                    })
                  }
                }
                if (cablesSort.indexOf(cab) == cablesSort.length - 1 && colDiams.nonEmpty ||
                  !cablesSort.filter(x => cablesSort.indexOf(x) > cablesSort.indexOf(cab)).exists(x => !usedInsert.contains(x.cable_id)) && colDiams.nonEmpty && step != 1){
                  totalRows += 1
                  rowDiams += colDiams.lastOption.getOrElse(0)
                }
              }
              else{
                error = "error: null diameter for cable " + cab.cable_id
              }
            }
          })


          while (error == "" && row <= node.nrows - 1){
            while (col <= node.ncolumns - 1){
              if (rowDiams.sum < nodeHeight){
                val diam = 30
                while (rowDiams.sum + diam <= nodeHeight){
                  y += -1 * rowDiams.lastOption.getOrElse(0d)
                  rowDiams += diam
                  totalRows += 1
                  val fillCount = nodeWidth / diam
                  (0.until(fillCount)).foreach(filler => {
                    x = xStart + col * nodeWidth + col * colP + filler * diam
                    val r = Image.rectangle(diam - p, diam - p)
                      .strokeColor(Color.red).strokeWidth(0.5).fillColor(Color.white).at(x + Math.ceil(diam / 2d) + p, y - diam / 2 - p)
                    val t = Image.text(if (numeric) (specCables.length + 1).toString else diam.toString)
                      .scale(diam * 1.5 / nodeWidth, diam * 1.5 / nodeWidth).at(x +  Math.ceil(diam / 2d) + p, y - diam / 2 - p)
                    pic = r.on(pic)
                    pic = t.on(pic)
                    fillerModules += diam

                    val moduleName = materials.find(_.name.contains("Уплотнительный модуль МКС " + diam.toString)) match {
                      case Some(value) => value.name.replace("Уплотнительный модуль ", "")
                      case _ => "Не найден"
                    }

                    specCables += EleCableSpec((specCables.length + 1).toString, "Модуль", moduleName)
                  })
                }
              }

              while (error == "" && rowDiams.sum + 5 <= nodeHeight){  //заполнение красных в нехватающую высоту после красных
                y += -1 * rowDiams.lastOption.getOrElse(0d)
                val diam = if (rowDiams.sum + 20 <= nodeHeight){
                  20
                }
                else if (rowDiams.sum + 15 <= nodeHeight){
                  15
                }
                else if (rowDiams.sum + 10 <= nodeHeight){
                  10
                }
                else{
                  5
                }
                x = xStart + col * nodeWidth + col * colP

                if (diam <= 10){
                  val r = Image.rectangle(nodeWidth - p, diam - p)
                    .strokeColor(Color.red).strokeWidth(0.5).fillColor(Color.white).at(x + nodeWidth / 2 + p, y - diam / 2 - p)
                  val t = Image.text(if (numeric) (specCables.length + 1).toString else diam.toString).scale(diam * 4.5 / nodeWidth, diam * 4.5 / nodeWidth).at(x + nodeWidth / 2 + p, y - diam / 2 - p)
                  pic = r.on(pic)
                  pic = t.on(pic)
                  fillerNoModules += diam

                  val moduleName = materials.find(x => x.name.contains("Глухой модуль МКС " + diam.toString) && x.name.contains(nodeWidth.toString)) match {
                    case Some(value) => value.name.replace("Глухой модуль ", "")
                    case _ => "Не найден"
                  }
                  specCables += EleCableSpec((specCables.length + 1).toString, "Модуль", moduleName)
                }
                else{
                  val fillCount = nodeWidth / diam
                  (0.until(fillCount)).foreach(filler => {
                    x = xStart + col * nodeWidth + col * colP + filler * diam
                    val r = Image.rectangle(diam - p, diam - p)
                      .strokeColor(Color.red).strokeWidth(0.5).fillColor(Color.white).at(x + Math.ceil(diam / 2d) + p, y - diam / 2 - p)
                    val t = Image.text(if (numeric) (specCables.length + 1).toString else diam.toString)
                      .scale(diam * 1.5 / nodeWidth, diam * 1.5 / nodeWidth).at(x +  Math.ceil(diam / 2d) + p, y - diam / 2 - p)
                    pic = r.on(pic)
                    pic = t.on(pic)

                    val moduleName = materials.find(_.name.contains("Уплотнительный модуль МКС " + diam.toString + "/")) match {
                      case Some(value) => value.name.replace("Уплотнительный модуль ", "")
                      case _ => "Не найден"
                    }
                    fillerModules += diam

                    specCables += EleCableSpec((specCables.length + 1).toString, "Модуль", moduleName)
                  })

                }
                totalRows += 1
                rowDiams += diam
              }

              col += 1
              y = yStart - row * nodeHeight
              rowDiams.clear()
              colDiams.clear()
            }
            row += 1
            col = 0
            y = yStart - row * nodeHeight
          }



          pic = pic.scale(scale, scale)

          val fileName = "node-" + new Date().getTime + ".png"
          var pathId = UUID.randomUUID().toString.substring(0, 12)
          var file = new File(App.Cloud.Directory + File.separator + pathId)
          while (file.exists()) {
            pathId = UUID.randomUUID().toString.substring(0, 8)
            file = new File(App.Cloud.Directory + File.separator + pathId)
          }
          file.mkdir()
          file = new File(App.Cloud.Directory + File.separator + pathId + File.separator + fileName)
          val fileUrl = App.Cloud.Url + "/" + pathId + "/" + fileName

//          val file = Files.createTempFile("image", ".png")
//          val fileUrl = file.toString

          pic.write[Png](file.toString)





          val sections = (node.nrows * node.ncolumns).toInt

          val rama = (materials.find(_.code == node.stock) match {
            case Some(value) =>
              spec += EleNodeSpec(value.name + ", секций " + (sections).toString + ", " + nodeHeight.toString + "x" + nodeWidth.toString, 1, value.singleWeight)
              value.name + ", секций " + (sections).toString + ", " +
              nodeHeight.toString + "x" + nodeWidth.toString +
              " мм, к-во 1 шт, вес " +  value.singleWeight.toString + " кг"
            case _ => "Не найдено по коду " + node.stock
          })
          specText += rama

          val plastina = (materials.find(_.name == ("Пластина анкерная " + nodeWidth.toString)) match {
            case Some(value) =>
              spec += EleNodeSpec(value.name, totalRows, Math.round((totalRows) * value.singleWeight * 100) / 100d)
              value.name + " мм, к-во " + (totalRows).toString + " шт, вес " + ((totalRows) * value.singleWeight).toString + " кг"
            case _ => "Не найдено по коду " + node.stock
          })
          specText += plastina

          (modules ++ fillerModules).groupBy(x => x).toList.sortBy(x => x._1).reverse.foreach(gr => {
            val module = materials.find(_.name.contains("Уплотнительный модуль МКС " + gr._1.toString)) match {
              case Some(value) =>
                spec += EleNodeSpec(value.name, gr._2.length, Math.round(gr._2.length * value.singleWeight * 100) / 100d)
                value.name + ", к-во  " + (gr._2.length).toString + " шт, вес " + ((gr._2.length) * value.singleWeight).toString + " кг"
              case _ => "Не найден уплотнительный модуль МКС " + gr._1.toString
            }
            specText += module
          })

          fillerNoModules.groupBy(x => x).toList.sortBy(x => x._1).reverse.foreach(gr => {
            val module = materials.find(x => x.name.contains("Глухой модуль МКС " + gr._1.toString) && x.name.contains(nodeWidth.toString)) match {
              case Some(value) =>
                spec += EleNodeSpec(value.name, gr._2.length, Math.round(gr._2.length * value.singleWeight * 100) / 100d)
                value.name + ", к-во  " + (gr._2.length).toString + " шт, вес " + (Math.round((gr._2.length) * value.singleWeight * 100) / 100d).toString + " кг"
              case _ => "Не найден глухой модуль МКС " + gr._1.toString
            }
            specText += module
          })



          val compression = (materials.find(_.name == ("Компрессионный блок МКС КБ " + nodeWidth.toString)) match {
            case Some(value) =>
              spec += EleNodeSpec(value.name, sections, sections * value.singleWeight)
              value.name + " мм, к-во " + (sections).toString + " шт, вес " + (Math.round((sections * value.singleWeight * 100) / 100d)).toString + " кг"
            case _ => "Не найдено по коду " + node.code
          })
          specText += compression

//          specText += "Смазка 1 шт"
          specText += "Смазка " + sections.toString + " шт"
          spec += EleNodeSpec("Смазка", 1, 0)

          val filePath = file.toString
          if (deleteFile){
            file.delete()
          }

          if (error == "") {
            EleNodePNG(node, cables, fileUrl, fileUrl, filePath, spec.toList, specText.toList, specCables.toList)
          }
          else if (step != 2){
            createNodeModulesPNG(project, node_id, materials, nodeModules, scale, numeric, step + 1)
          }
          else{
            EleNodePNG(node, cables, error, fileUrl, filePath, spec.toList, specText.toList, specCables.toList)
          }

        case _ =>
          EleNodePNG(
            EleNode(0, "", 0, 0, 0, 0, 0, "", "", "", "", 0, 0, 0, 0, 0, 0, 0, "", 0, 0, "", "", ""),
            List.empty[EleCable],
            "",
            "",
            "",
            spec.toList,
            specText.toList,
            List.empty[EleCableSpec]
          )
      }
    }
    catch {
      case e: Throwable => EleNodePNG(
        EleNode(0, "", 0, 0, 0, 0, 0, "", "", "", "", 0, 0, 0, 0, 0, 0, 0, "", 0, 0, "", "", ""),
        List.empty[EleCable],
        "alert: " + e.toString,
        "",
        "",
        List.empty[EleNodeSpec], List.empty[String],
        List.empty[EleCableSpec]
      )
    }
  }
  def checkNodeModulesPNG(project: String, node: EleNode, nodeModules: List[EleNodeModule]): String = {
    "ok"
  }

  //  def checkNodeModulesPNG(project: String, node: EleNode, nodeModules: List[EleNodeModule]): String = {
//    try{
//      val cables = getEleNodeCables(project, node.node_id)
//      var error = "ok"
//      var totalRows = 0
//      val modules = ListBuffer.empty[Double]
//      val fillerModules = ListBuffer.empty[Double]
//
//      val split = node.descr.split("\n").lastOption.getOrElse("").replace("х", "x")
//      val wh = split.split("x")
//      val nodeWidth = wh(0).toIntOption.getOrElse(0)
//      val nodeHeight = wh(1).toIntOption.getOrElse(0)
//
//      if (nodeHeight == 0 || nodeWidth == 0) {
//        error = "error: node height or width is zero"
//      }
//
//      val xStart = -1 * nodeWidth * node.ncolumns / 2d
//      val yStart = nodeHeight * node.nrows / 2d
//
//      var x = xStart
//      var y = yStart
//      var col = 0
//      var row = 0
//
//      val colDiams = ListBuffer.empty[Double]
//      val rowDiams = ListBuffer.empty[Double]
//
//      val cablesSort = cables.sortBy(x => (x.diamModule(nodeModules), x.cable_id)).reverse
//      cablesSort.foreach(cab => {
//        val diam = cab.diamModule(nodeModules)
//        modules += diam
//        if (diam > 0){
//          if (colDiams.nonEmpty && colDiams.lastOption.getOrElse(0) != diam){
//            if (colDiams.sum + colDiams.last <= nodeWidth){
//              val fillDiam = colDiams.last
//              val fillCount = nodeWidth / fillDiam - colDiams.length
//              (0.until(fillCount.toInt)).foreach(filler => {
//                x = xStart + col * nodeWidth + colDiams.length * fillDiam + filler * fillDiam
//                fillerModules += fillDiam
//              })
//            }
//            y += -1 * colDiams.last
//            rowDiams += colDiams.last
//            colDiams.clear()
//            totalRows += 1
//          }
//          else if (colDiams.nonEmpty && (colDiams.sum + diam) > nodeWidth){
//            y += -1 * colDiams.last
//            rowDiams += colDiams.last
//            colDiams.clear()
//            totalRows += 1
//          }
//
//          if (rowDiams.sum + diam > nodeHeight){
//            if (col < node.ncolumns - 1){
//              col += 1
//            }
//            else if (row < node.nrows - 1){
//              row += 1
//              col = 0
//            }
//            else{
//              error = "error: not enough space for cables, placed " + cablesSort.indexOf(cab).toString + " of " + cablesSort.length.toString
//            }
//            y = yStart - row * nodeHeight
//            rowDiams.clear()
//            colDiams.clear()
//          }
//
//          if (colDiams.isEmpty){
//            x = xStart + col * nodeWidth
//          }
//          else{
//            x = xStart + col * nodeWidth + colDiams.length * diam
//          }
//          colDiams += diam
//          if (cablesSort.indexOf(cab) == cablesSort.length - 1 && colDiams.nonEmpty && colDiams.sum + diam <= nodeWidth){
//            val fillCount = nodeWidth / diam - colDiams.length
//            (0.until(fillCount.toInt)).foreach(filler => {
//              x = xStart + col * nodeWidth + colDiams.length * diam + filler * diam
//              fillerModules += diam
//            })
//          }
//          if (cablesSort.indexOf(cab) == cablesSort.length - 1 && colDiams.nonEmpty){
//            totalRows += 1
//          }
//        }
//        else{
//          error = "error: null diameter for cable " + cab.cable_id
//        }
//      })
//
//      error
//    }
//    catch {
//      case e: Throwable => "error: " + e.toString
//    }
//  }
  def createNodeModulesPDF(project: String, node_id: Int): String ={
    try{
      val fileName = "node-" + new Date().getTime + ".pdf"
      var pathId = UUID.randomUUID().toString.substring(0, 12)
      var file = new File(App.Cloud.Directory + File.separator + pathId)
      while (file.exists()) {
        pathId = UUID.randomUUID().toString.substring(0, 8)
        file = new File(App.Cloud.Directory + File.separator + pathId)
      }
      file.mkdir()
      file = new File(App.Cloud.Directory + File.separator + pathId + File.separator + fileName)
      val fileUrl = App.Cloud.Url + "/" + pathId + "/" + fileName

      val node = createNodeModulesPNG(project, node_id, numeric = true)
      val writer = new PdfWriter(file.toString)
      val pdfDoc = new PdfDocument(writer)
      val doc = new Document(pdfDoc)
      val gostFont = PdfFontFactory.createFont(FontProgramFactory.createFont("fonts/GOSTtypeA.ttf"), PdfEncodings.IDENTITY_H, EmbeddingStrategy.PREFER_NOT_EMBEDDED)
      doc.setFont(gostFont)
      doc.setFontSize(10F)


      val header = new Paragraph("Узел: " + node.node.node + ", " + node.node.descr)
      header.setFontSize(12F)
      header.setBold()
      header.setTextAlignment(TextAlignment.CENTER)
      doc.add(header)

      val tableOfTables = new Table(3)
      tableOfTables.setHorizontalAlignment(HorizontalAlignment.CENTER)
      val tableColumnWidths = Array(15F, 50F, 80F)
      node.specCables.grouped((Math.ceil(node.specCables.length / 3d) + 1).toInt).foreach(cableGroup => {
        val table = new Table(tableColumnWidths)
        cableGroup.foreach(cable => {

          val cellPos = new Cell()
          val cellCable = new Cell()
          val cellModule = new Cell()

          cellPos.add(new Paragraph(cable.pos))
          cellCable.add(new Paragraph(cable.cable))
          cellModule.add(new Paragraph(cable.module))

          table.addCell(cellPos)
          table.addCell(cellCable)
          table.addCell(cellModule)
        })
        val tableCell = new Cell()
        tableCell.add(table)
        tableCell.setBorder(Border.NO_BORDER)
        tableOfTables.addCell(tableCell)
      })
      doc.add(tableOfTables)

      val headerCommon = new Paragraph("Сводные данные")
      headerCommon.setFontSize(12F)
      headerCommon.setMarginTop(20F)
      headerCommon.setItalic()
      doc.add(headerCommon)
      node.specText.foreach(text => {
        val specText = new Paragraph(text)
        specText.setMarginTop(0F)
        specText.setMarginBottom(0F)
        specText.setItalic()
        doc.add(specText)
      })

      doc.add(new AreaBreak())

      val imageData = ImageDataFactory.create(node.png_path)
      val image = new com.itextpdf.layout.element.Image(imageData)
      image.setHorizontalAlignment(HorizontalAlignment.CENTER)
      if (node.node.ncolumns > 2){
        image.setRotationAngle(-90 * Math.PI / 180)
      }
      doc.add(image.setAutoScale(true))
      doc.close()
      fileUrl
    }
    catch{
      case e: Throwable => "error: " + e.toString
    }

  }

}
