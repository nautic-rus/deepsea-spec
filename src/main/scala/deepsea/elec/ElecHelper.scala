package deepsea.elec

import deepsea.database.DBManager
import deepsea.database.DatabaseManager.{GetMongoConnection, GetOracleConnection}
import deepsea.elec.ElecManager.{CableBoxesBySystem, ElecCable, TrayBySystem, TraysBySystem}
import deepsea.pipe.PipeManager.Material
import local.common.Codecs
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.model.Filters.{and, equal}

import scala.collection.mutable.ListBuffer
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.io.Source

trait ElecHelper extends Codecs{
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
    GetMongoConnection() match {
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

  def getTraysBySystem(project: String, docNumber: String): ListBuffer[TraysBySystem] = {
    val res = ListBuffer.empty[TraysBySystem];
    GetOracleConnection(project) match {
      case Some(c) =>
        val doc = docNumber.split("-").takeRight(2).mkString("-")
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
      case _ =>
    }
    res
  }

  def getTotalTraysBySystem(project: String, docNumber: String): ListBuffer[TrayBySystem] = {
    val res = ListBuffer.empty[TrayBySystem];
    GetOracleConnection(project) match {
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
    res
  }

  def getCableBoxesBySystem(project: String, docNumber: String): ListBuffer[CableBoxesBySystem] = {
    val res = ListBuffer.empty[CableBoxesBySystem];
    GetOracleConnection(project) match {
      case Some(c) =>
        val doc = docNumber.split("-").takeRight(2).mkString("-")
        val query = Source.fromResource("queries/elecTotalCableBoxesInSystem.sql").mkString.replaceAll(":docNumber", "'" + doc + "'");
        val s = c.prepareStatement(query);
        val rs = s.executeQuery();
        val materials: List[Material] = getMaterials();
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
            materials.find(x => {x.code == code || name.contains(x.name)}) match {
              case Some(value) => value
              case _ => Material()
            }
          )
        }
        rs.close()
        s.close()
        c.close()
      case _ =>
    }
    res
  }

}
