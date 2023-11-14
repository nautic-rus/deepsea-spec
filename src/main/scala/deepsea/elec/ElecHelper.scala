package deepsea.elec

import breeze.linalg.DenseMatrix
import deepsea.database.DBManager
import deepsea.database.DBManager.RsIterator
import deepsea.elec.ElecManager._
import deepsea.esp.EspManager.{DeviceEspObject, espObjectsCollectionName}
import deepsea.esp.EspManagerHelper
import deepsea.pipe.PipeManager.{Material, SpoolLock}
import local.common.Codecs
import local.common.DBRequests.{MountItem, findWorkshopMaterialContains, retrieveAllMaterialsByProject}
import local.domain.WorkShopMaterial
import local.ele.CommonEle.EleComplect
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.{and, equal}
import io.circe.parser._
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.syntax.EncoderOps
import io.circe._
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.generic.semiauto._
import java.util.Date
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source

trait ElecHelper extends Codecs with EspManagerHelper {
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
        stmt.close()
        rs.close()
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

  def getBlocks(project: String): List[Block] = {
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val q = "select * from block"
        val res = RsIterator(stmt.executeQuery(q)).map(rs => {
          Block(
            Option(rs.getString("CODE")).getOrElse(""),
            Option(rs.getString("DESCRIPTION")).getOrElse(""),
          )
        }).toList
        stmt.close()
        connection.close()
        res
      case _ => List.empty[Block]
    }
  }
  def getZones(project: String): List[Zone] = {
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val q = "select zn.name, zl.descr from zone zn, zone_lang zl where zn.oid = zl.zone and zl.lang = -2"
        val res = RsIterator(stmt.executeQuery(q)).map(rs => {
          Zone(
            Option(rs.getString("NAME")).getOrElse(""),
            Option(rs.getString("DESCRIPTION")).getOrElse(""),
          )
        }).toList
        stmt.close()
        connection.close()
        res
      case _ => List.empty[Zone]
    }
  }
  def getSystems(project: String): List[System] = {
    DBManager.GetOracleConnection(project) match {
      case Some(connection) =>
        val stmt = connection.createStatement()
        val q = "select st.name, sl.descr from systems st, systems_lang sl where sl.system = st.oid and sl.lang = -2"
        val res = RsIterator(stmt.executeQuery(q)).map(rs => {
          System(
            Option(rs.getString("NAME")).getOrElse(""),
            Option(rs.getString("DESCRIPTION")).getOrElse(""),
          )
        }).toList
        stmt.close()
        connection.close()
        res
      case _ => List.empty[System]
    }
  }
  def getEleComplects(project: String): List[EleComplect] = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollection: MongoCollection[EleComplect] = mongo.getCollection("eleComplects")
        Await.result(espCollection.find().toFuture(), Duration(10, SECONDS)) match {
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
  def deleteEleComplect(drawing: String): Unit = {
    DBManager.GetMongoConnection() match {
      case Some(mongo) =>
        val espCollection: MongoCollection[EleComplect] = mongo.getCollection("eleComplects")
        Await.result(espCollection.deleteOne(equal("drawingId", drawing)).toFuture(), Duration(10, SECONDS))
      case _ => None
    }
  }
}
