package mamonov

import deepsea.database.DBManager
import deepsea.elec.ElecManager.CableRoute
import deepsea.pipe.PipeManager.Material
import local.pdf.ru.ele.EleTrayCableBoxReportRu.getMaterials
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ListBuffer
import scala.io.Source

class eleccables extends AnyFunSuite {
  val res = ListBuffer.empty[CableRoute];
  DBManager.GetOracleConnection("P701") match {
    case Some(c) =>
      val doc = "170701-888-130Э4";
      val materials: List[Material] = getMaterials();
//      val doc = docNumber.split("-").takeRight(2).mkString("-");
      val query = Source.fromResource("queries/elecCables.sql").mkString.replaceAll(":docNumber", "'%" + doc + "%'");
      val s = c.createStatement();
      val rs = s.executeQuery(query);
      try {
        while (rs.next()) {
          val code = Option(rs.getString("STOCK_CODE")).getOrElse("");
          val name = Option(rs.getString("CODE")).getOrElse("");
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
            Option(rs.getString("ROUTE_AREA")).getOrElse(""),
            Option(rs.getString("ROUTE_AREA_ID")).getOrElse(""),
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
  val a = 0;
}
