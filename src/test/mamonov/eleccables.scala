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
  DBManager.GetOracleConnection("N002") match {
    case Some(c) =>
      val docNumber = "170701-888-130Э4";
      val doc = docNumber.split("-").takeRight(2).mkString("-");
      val query = Source.fromResource("queries/elecCables.sql").mkString.replaceAll(":docNumber", "'" + doc + "'");
      val s = c.prepareStatement(query);
      val rs = s.executeQuery();
      val materials: List[Material] = getMaterials();
      while (rs.next()) {
        val code = Option(rs.getString("STOCKCODE")).getOrElse("");
        val name = Option(rs.getString("CODE")).getOrElse("");
        res += CableRoute(
          Option(rs.getString("SYSTEM")).getOrElse(""),
          name,
          Option(rs.getString("DESCR")).getOrElse(""),
          Option(rs.getString("NOM_SECTION")).getOrElse(""),
          Option(rs.getInt("O_DIAMETER")).getOrElse(0),
          Option(rs.getString("SEG_CODE")).getOrElse(""),
          Option(rs.getDouble("F_ROUT")).getOrElse(0),
          Option(rs.getDouble("LENGTH")).getOrElse(0.0),
          Option(rs.getString("FROM_NODE")).getOrElse(""),
          Option(rs.getString("FROM_NODE_DESC")).getOrElse(""),
          Option(rs.getString("FROM_NODE_ROOM")).getOrElse(""),
          Option(rs.getString("FROM_ELEM")).getOrElse(""),
          Option(rs.getString("FROM_ELEM_DESC")).getOrElse(""),
          Option(rs.getString("TO_NODE")).getOrElse(""),
          Option(rs.getString("TO_NODE_DESC")).getOrElse(""),
          Option(rs.getString("TO_NODE_ROOM")).getOrElse(""),
          Option(rs.getString("TO_ELEM")).getOrElse(""),
          Option(rs.getString("TO_ELEM_DESC")).getOrElse(""),
          Option(rs.getString("CAB_ROUTE_AREA")).getOrElse(""),
          code,
          materials.find(x => {
            x.code == code || name.contains(x.name);
          }) match {
            case Some(value) => value;
            case _ => Material();
          }
        )
      }
      rs.close();
      s.close();
      c.close();
    case _ =>
  }
  res.toList;
  val a = 0;
}
