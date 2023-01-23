package deepsea.elec

import deepsea.database.DBManager
import deepsea.elec.ElecManager.ElecCable

import scala.collection.mutable.ListBuffer

trait ElecHelper {
  def getCablesInfo(project: String): List[ElecCable] ={
    val cables = ListBuffer.empty[ElecCable]
    DBManager.GetOracleConnection(project) match {
      case Some(oracleConnection) =>
        val stmt = oracleConnection.createStatement()
        val query = "SELECT * FROM V_CABLE"
        val rs = stmt.executeQuery(query)
        while (rs.next()){
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

}
