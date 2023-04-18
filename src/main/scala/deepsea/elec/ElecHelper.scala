package deepsea.elec

import deepsea.database.DBManager
import deepsea.database.DatabaseManager.{GetMongoConnection, GetOracleConnection}
import deepsea.elec.ElecManager.{CableBoxesBySystem, CableRoute, ElecAngle, ElecCable, NodeConnect, TrayBySystem, TraysBySystem}
import deepsea.pipe.PipeManager.Material
import local.common.Codecs
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.{and, equal, not}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source

trait ElecHelper extends Codecs {
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

  def getMaterials(): List[Material] = {
    DBManager.GetMongoConnection() match {
      case Some(mongoData) =>
        val materialsNCollectionName = "materials-n";
        val materialsCollection: MongoCollection[Material] = mongoData.getCollection(materialsNCollectionName);

        Await.result(materialsCollection.find[Material]().toFuture(), Duration(30, SECONDS)) match {
          case dbMaterials => dbMaterials.toList;
          case _ => List.empty[Material]
        }
      case _ => List.empty[Material]
    }
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
    val elecAngles = getElecAngles
    DBManager.GetOracleConnection(project) match {
      case Some(c) =>
        val doc = docNumber.split("-").takeRight(2).mkString("-");
        val query = Source.fromResource("queries/elecTraysInSystem.sql").mkString.replaceAll(":docNumber", "'" + doc + "'");
        val s = c.prepareStatement(query);
        val rs = s.executeQuery();
        val materials: List[Material] = getMaterials();
        while (rs.next()) {
          val code = Option(rs.getString("STOCK_CODE")).getOrElse("");
          res += new TraysBySystem(
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

        res.map(_.material).distinct.foreach(e => {
          val elecAngle = elecAngles.find(a => e.name.contains(a.name)) match {
            case Some(angleValue) =>
              materials.find(_.code == angleValue.code) match {
                case Some(material) => material
                case _ => Material()
              }
            case _ => Material()
          }
          if (elecAngle.code != ""){

            var totalHeight: Double = 0.0;
            res.groupBy(x => x.stockCode).foreach(gr => {
              val len = gr._2.map(_.length).sum;
              totalHeight += 0.3 * Math.ceil(len / 1.2) * 2;
            })
            val wght: Double = totalHeight * elecAngle.singleWeight;
            val totalWeight: String = if (wght < 0.01) " 0.01" else String.format("%.2f", wght);

            res += new TraysBySystem(
              doc,
              0,
              "",
              0,
              totalWeight.toDoubleOption.getOrElse(0.0),
              0.0,
              0.0,
              0.0,
              "",
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
        val materials: List[Material] = getMaterials();
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
        val materials: List[Material] = getMaterials();
        val rout = ListBuffer.empty[NodeConnect];

        val s = c.createStatement();

        val queryN = Source.fromResource("queries/elecKeyNodes.sql").mkString
        val rsn = s.executeQuery(queryN)
        try {
          while (rsn.next()) {
            rout += NodeConnect(
              Option(rsn.getInt("SEQID")).getOrElse(0),
              Option(rsn.getInt("COUNT")).getOrElse(0))
          }
        }
        catch {
          case e: Exception => println(e.toString)
        }

        val query = Source.fromResource("queries/elecCables.sql").mkString.replaceAll(":docNumber", "'%" + docNumber + "%'");
        val rs = s.executeQuery(query);
        try {
          while (rs.next()) {
            val code = Option(rs.getString("STOCK_CODE")).getOrElse("");
            val name = Option(rs.getString("CODE")).getOrElse("");
            val cab_route_area = Option(rs.getString("ROUTE_AREA")).getOrElse("");
//            val cab_route_area_id = Option(rs.getString("ROUTE_AREA_ID")).getOrElse("");
//
//            val nodes = ListBuffer.empty[Int]
//            val routeArea = cab_route_area.split('^').toList
//            val routeAreaId = cab_route_area_id.split(',').toList
//            val nodesId = rout.filter(x => routeAreaId.contains(x.id.toString))
//
//            val filterRout = nodesId.filter(_.count > 2)
//            filterRout.foreach(node => {
//              val i = rout.indexOf(node);
//              if (i == 0 || i == filterRout.size - 1) {
//                nodes += node.id
//              } else {
//                nodes += rout(i - 1).id;
//                nodes += rout(i + 1).id;
//              }
//            })

//            val paramRouteArea = ListBuffer.empty[String]
//
//            routeAreaId.foreach(node => {
//              val i = rout.indexOf(node);
//              if (node == nodes(i).toString) {
//                paramRouteArea += routeArea(i)
//              }
//            })

            res += CableRoute(
              Option(rs.getString("SYSTEM")).getOrElse(""),
              name,
              Option(rs.getString("DESCRIPTION")).getOrElse(""),
              Option(rs.getString("NOM_SECTION")).getOrElse(""),
              Option(rs.getInt("DIAMETER")).getOrElse(0),
              Option(rs.getString("SEG_CODE")).getOrElse(""),
              Option(rs.getDouble("F_ROUT")).getOrElse(0),
              Option(rs.getDouble("LENGTH")).getOrElse(0.0),
              Option(rs.getDouble("EXT_LEN_1")).getOrElse(0.0),
              Option(rs.getDouble("EXT_LEN_2")).getOrElse(0.0),
              Option(rs.getString("FROM_SYSTEM")).getOrElse(""),
              Option(rs.getString("FROM_EQ_ID")).getOrElse(""),
              Option(rs.getString("FROM_EQ_DESC")).getOrElse(""),
              Option(rs.getString("FROM_EQ")).getOrElse(""),
              Option(rs.getString("FROM_STOCK_CODE")).getOrElse(""),
              Option(rs.getDouble("FROM_X")).getOrElse(0.0),
              Option(rs.getDouble("FROM_Y")).getOrElse(0.0),
              Option(rs.getDouble("FROM_Z")).getOrElse(0.0),
              Option(rs.getString("FROM_ZONE")).getOrElse(""),
              Option(rs.getString("FROM_ZONE_DESC")).getOrElse(""),
              Option(rs.getString("TO_SYSTEM")).getOrElse(""),
              Option(rs.getString("TO_EQ_ID")).getOrElse(""),
              Option(rs.getString("TO_EQ_DESC")).getOrElse(""),
              Option(rs.getString("TO_EQ")).getOrElse(""),
              Option(rs.getString("TO_STOCK_CODE")).getOrElse(""),
              Option(rs.getDouble("TO_X")).getOrElse(0.0),
              Option(rs.getDouble("TO_Y")).getOrElse(0.0),
              Option(rs.getDouble("TO_Z")).getOrElse(0.0),
              Option(rs.getString("TO_ZONE")).getOrElse(""),
              Option(rs.getString("TO_ZONE_DESC")).getOrElse(""),
              cab_route_area,
              code,
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


        rs.close();
        s.close();
        c.close();
      case _ =>
    }
    res.toList;
  }
}
