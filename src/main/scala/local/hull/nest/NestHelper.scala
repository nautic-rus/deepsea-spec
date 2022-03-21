package local.hull.nest

import local.common.DBRequests.{insertNestsLock, listToSqlString, retrieveNestsLocksByProject}
import local.hull.nest.CommonNest.{Nest, NestIdBlock, NestLock, NestMaterial}
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer

trait NestHelper {


  private def allPlateNestSQL(): String = "select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%'\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)"

  private def plateNestByMaterial(mats: String) = s"select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%'\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)\nwhere MATERIAL in(${mats})"

  private def plateNestByMaterialAndDim(par: String) = s"select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%'\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)\nwhere (MATERIAL||THICKNESS||NEST_LENGTH||NEST_WIDTH) in(${par}) "

  private def plateNestByBlocks(blocks: String) = s"select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%' and BLOCK_CODE in(${blocks})\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)"

  private def nestBlockSql(): String = "select distinct NEST_ID,  BLOCK_CODE from V_PRD_PART_NEST_LIST order by nest_id"

  private def materialListAllSQL(): String = "select distinct MATERIAL, THICKNESS,NEST_LENGTH,NEST_WIDTH from V_PRD_PART_NEST_LIST where  NEST_ID like 'MU%' order by MATERIAL,THICKNESS"

  private def materialListByBlocksSQL(blocks: String): String = s"select distinct MATERIAL, THICKNESS,NEST_LENGTH,NEST_WIDTH from V_PRD_PART_NEST_LIST where  NEST_ID like 'MU%' and BLOCK_CODE in(${blocks})  order by MATERIAL,THICKNESS"

  private def blocksListSql(): String = "select distinct BLOCK_CODE from V_PRD_PART_NEST_LIST where  NEST_ID like 'MU%'  order by BLOCK_CODE"

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
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
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

  def plateNestByMaterials(project: String, materials: List[NestMaterial]): List[Nest] = {
    val mats: String = {
      val b = ListBuffer.empty[String]
      materials.foreach(m => b += m.MATERIAL)
      listToSqlString(b.toList)
    }
    val nestsBlocks: List[NestIdBlock] = getNestBlock(project)
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[Nest]
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(plateNestByMaterial(mats))
          while (rs.next()) {
            val id = Option(rs.getString("ID")).getOrElse("")
            buffer += Nest(
              id,
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0),
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
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

  def plateNestByMaterialsAndDims(project: String, materials: List[NestMaterial]): List[Nest] = {
    val par: String = listToSqlString({
      val buf = ListBuffer.empty[String]
      materials.foreach(i => {
        var ret = i.MATERIAL
        if (Math.abs(i.THICKNESS.toInt - i.THICKNESS) == 0) {
          ret += i.THICKNESS.toInt
        } else {
          ret += f"${i.THICKNESS}%.1f"
        }
        ret += i.NEST_LENGTH
        ret += i.NEST_WIDTH
        buf += ret
      })
      buf.toList
    })
    val nestsBlocks: List[NestIdBlock] = getNestBlock(project)
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[Nest]
          val stmt: Statement = connection.createStatement()
          val sql = plateNestByMaterialAndDim(par)
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            val id = Option(rs.getString("ID")).getOrElse("")
            buffer += Nest(
              id,
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0),
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
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

  def plateNestByBlocks(project: String, blocks: List[String]): List[Nest] = {
    val par: String = listToSqlString(blocks)

    val nestsBlocks: List[NestIdBlock] = getNestBlock(project)

    val locks: List[NestLock] = retrieveNestsLocksByProject(project)

    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[Nest]
          val stmt: Statement = connection.createStatement()
          val sql = plateNestByBlocks(par)
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            val id = Option(rs.getString("ID")).getOrElse("")
            val li: (NestLock, Boolean) = calculateLockInfo(id, locks)
            buffer += Nest(
              id,
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0),
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
              Option(rs.getInt("NUM_EQ_NEST")).getOrElse(0),
              Option(rs.getDouble("PARTS_WEIGHT")).getOrElse(0),
              Option(rs.getDouble("DENSITY")).getOrElse(0),
              Option(rs.getDouble("USAGE")).getOrElse(0),
              nestsBlocks.filter(s => s.nestID.equals(id)).map(_.block).mkString(";"),
              li._2,
              li._1
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

  def genMateriaAlllList(project: String): List[NestMaterial] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[NestMaterial]
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(materialListAllSQL())
          while (rs.next()) {
            buffer += NestMaterial(
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0),
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
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
            List.empty[NestMaterial]
        }
      }
      case None => List.empty[NestMaterial]
    }
  }

  def genMaterialByBlock(project: String, blockList: List[String]): List[NestMaterial] = {
    val blocks = listToSqlString(blockList)

    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[NestMaterial]
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(materialListByBlocksSQL(blocks))
          while (rs.next()) {
            buffer += NestMaterial(
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0),
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
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
            List.empty[NestMaterial]
        }
      }
      case None => List.empty[NestMaterial]
    }
  }

  def genBlocks(project: String): List[String] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[String]
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(blocksListSql())
          while (rs.next()) {
            buffer += Option(rs.getString("BLOCK_CODE")).getOrElse("")
          }
          rs.close()
          stmt.close()
          connection.close()
          buffer.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[String]
        }
      }
      case None => List.empty[String]
    }
  }

  def insertLock(project: String, nestId: String, user: String): Unit = {
    insertNestsLock(project, nestId, user)
  }

  private def calculateLockInfo(nestId: String, locks: List[NestLock]): (NestLock, Boolean) = {
    val locksData = locks.filter(p => p.nestId.equals(nestId))
    if (locksData.nonEmpty) {
      val isLock: Boolean = locksData.length % 2 != 0
      val nl: NestLock = locksData.maxBy(s => s.date)
      Tuple2(nl, isLock)
    } else {
      Tuple2(NestLock(), false)
    }
  }


}
