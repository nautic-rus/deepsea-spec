package local.hull.nest

import local.hull.nest.CommonNest.{Nest, NestIdBlock}
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer

trait NestHelper {


  private def allPlateNestSQL(): String = "select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%'\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)"

  private def nestBlockSql(): String = "select distinct NEST_ID,  BLOCK_CODE from V_PRD_PART_NEST_LIST order by nest_id"

  private def getNestBlock(project: String): List[NestIdBlock] = {

    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[NestIdBlock]
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(nestBlockSql())
          while (rs.next()) {
            buffer += NestIdBlock(
              Option(rs.getString("NEST_ID")).getOrElse(""),
              Option(rs.getString("BLOCK_CODE")).getOrElse("")
            )
          }
          rs.close()
          stmt.close()
          connection.close()
          buffer.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[NestIdBlock]
        }
      }
      case None => List.empty[NestIdBlock]
    }
  }


  def allPlateNest(project: String): List[Nest] = {
    val nestsBlocks: List[NestIdBlock] = getNestBlock(project)
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[Nest]
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(allPlateNestSQL())
          while (rs.next()) {
            val id = Option(rs.getString("ID")).getOrElse("")
            buffer += Nest(
              id,
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0),
              Option(rs.getDouble("NEST_LENGTH")).getOrElse(0),
              Option(rs.getDouble("NEST_WIDTH")).getOrElse(0),
              Option(rs.getInt("NUM_EQ_NEST")).getOrElse(0),
              Option(rs.getDouble("PARTS_WEIGHT")).getOrElse(0),
              Option(rs.getDouble("DENSITY")).getOrElse(0),
              Option(rs.getDouble("USAGE")).getOrElse(0),
              nestsBlocks.filter(s => s.nestID.equals(id)).map(_.block).mkString(";")
            )
          }
          rs.close()
          stmt.close()
          connection.close()
          buffer.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[Nest]
        }
      }
      case None => List.empty[Nest]
    }
  }


}
