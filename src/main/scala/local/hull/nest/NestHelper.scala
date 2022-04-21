package local.hull.nest

import local.common.DBRequests.{insertNestsLock, listToSqlString, retrieveNestsLocksByProject}
import local.hull.bill.BillManager.ForanScrap
import local.hull.bill.{BillHelper, BillManager}
import local.hull.nest.CommonNest.{Nest, Nest2, NestIdBlock, NestLock, NestMaterial, calculateWeightKG}
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer

trait NestHelper extends BillHelper {


  private def allPlateNestSQL(): String = "select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%'\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)"


  private def allPlateNestSQL2(): String = "select \nOID,\nNESTID,\nKPL,\nLENGTH,\nWIDTH,\nTHICKNESS,\nMAT,\nDENSITY,\nlistagg(CODE,';') within group( order by CODE ) AS BLOCKS,\nNUM_PRT,\nNUM_EQ_NEST,\nAREA,\n(AREA*THICKNESS*DENSITY) AS PARTS_WEIGHT,\n(LENGTH*WIDTH*THICKNESS*DENSITY/1000000) AS GROSS_WEIGHT,\n((AREA*THICKNESS*DENSITY)/(LENGTH*WIDTH*THICKNESS*DENSITY/1000000))*100 as USAGE,\nPARENTNESTID,\nCHILDNESTID,\nCHILDKPL,\nCHILDLENGTH,\nCHILDWIDTH,\nCHILDAREAM2,\n(CHILDAREAM2*THICKNESS*DENSITY) AS CHILDWEIGHT\nfrom\n(\nselect distinct \nB.CODE,\nGET_NEST_ID(N.OID, N.ID_SEQ_BLOCK) as NESTID,\nSTD.KPL,\nSTD.LENGTH,\nSTD.WIDTH,\nSTD.THICKNESS,\nM.CODE as MAT,\nM.DENSITY,\nN.NUM_PRT,\nN.NUM_EQ_NEST+1 as NUM_EQ_NEST,\nN.AREA,\nN.OID as OID,\nGET_NEST_ID(\n(select FROM_NEST_OID from PRD_STD_PLT_ENCLOS where STD_PLATE_OID=N.COD_PLT) ,\n(select ID_SEQ_BLOCK from PRD_NEST_GEN where OID =(select FROM_NEST_OID from PRD_STD_PLT_ENCLOS where STD_PLATE_OID=N.COD_PLT))\n) as PARENTNESTID,\nGET_NEST_ID(\n(select OID from PRD_NEST_GEN where COD_PLT = (select STD_PLATE_OID from PRD_STD_PLT_ENCLOS where FROM_NEST_OID=N.OID)),\n(select ID_SEQ_BLOCK from PRD_NEST_GEN where COD_PLT = (select STD_PLATE_OID from PRD_STD_PLT_ENCLOS where FROM_NEST_OID=N.OID))\n) as CHILDNESTID,\n(select KPL from STD_PLATE where OID=(select STD_PLATE_OID from PRD_STD_PLT_ENCLOS where FROM_NEST_OID=N.OID)) as CHILDKPL,\n(select SLENGTH from PRD_STD_PLT_ENCLOS where FROM_NEST_OID=N.OID) as CHILDLENGTH,\n(select SWIDTH from PRD_STD_PLT_ENCLOS where FROM_NEST_OID=N.OID) as CHILDWIDTH,\n(select AREA_M2 from PRD_STD_PLT_ENCLOS where FROM_NEST_OID=N.OID) as CHILDAREAM2\nfrom PRD_NEST_GEN N, BLOCK B, PRD_NEST_PART NP, PRD_PART PP, STD_PLATE STD, MATERIAL M\nwhere \nNP.NESTING_OID=N.OID AND\nPP.OID=NP.PART_OID AND\nPP.BLOCK_OID=B.OID AND\nSTD.OID=N.COD_PLT AND\nSTD.MATERIAL_OID=M.OID AND\nN.NEST_TYPE=0\n)\ngroup by \nOID,\nNESTID,\nKPL,\nLENGTH,\nWIDTH,\nTHICKNESS,\nMAT,\nDENSITY,\nNUM_PRT,\nNUM_EQ_NEST,\nAREA,\nPARENTNESTID,\nCHILDNESTID,\nCHILDKPL,\nCHILDLENGTH,\nCHILDWIDTH,\nCHILDAREAM2\norder by\nNESTID"

  private def plateNestByMaterial(mats: String) = s"select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%'\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)\nwhere MATERIAL in(${mats})"

  //private def plateNestByMaterialAndDimOLD(par: String) = s"select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%'\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)\nwhere (MATERIAL||THICKNESS||NEST_LENGTH||NEST_WIDTH) in(${par}) "
  //private def plateNestByMaterialAndDim(par: String) = s"select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%'\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)\nwhere (MATERIAL||THICKNESS||ROUND(NEST_LENGTH)||ROUND(NEST_WIDTH)) in(${par}) "
  private def plateNestByMaterialAndDim(par: String) = s"select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%'\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)\nwhere (MATERIAL||THICKNESS||TRUNC(NEST_LENGTH,0)||TRUNC(NEST_WIDTH,0)) in(${par}) "

  private def plateNestByBlocks(blocks: String) = s"select  ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,PARTS_WEIGHT,DENSITY,USAGE\nfrom(\nselect \n N.NEST_ID as ID, N.MATERIAL,N.THICKNESS,N.NEST_LENGTH,N.NEST_WIDTH,N.NUM_EQ_NEST,N.PARTS_WEIGHT,\n M.DENSITY,\n N.PARTS_WEIGHT/(((N.NEST_LENGTH*N.NEST_WIDTH*N.THICKNESS)/1000000000)*M.DENSITY*1000)*100 as USAGE\nfrom \n(\nselect NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST, SUM( WEIGHT_UNITS) as PARTS_WEIGHT\nfrom\n    (\n    select NEST_ID, MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST,  WEIGHT_UNIT*NUM_PART_NEST as WEIGHT_UNITS\n    from V_PRD_PART_NEST_LIST \n    where NEST_ID like 'MU%' and BLOCK_CODE in(${blocks})\n    )\nGROUP BY NEST_ID,MATERIAL,THICKNESS,NEST_LENGTH,NEST_WIDTH,NUM_EQ_NEST\n) N, MATERIAL M\nwhere N.MATERIAL=M.CODE\norder by N.NEST_ID\n)"

  private def nestBlockSql(): String = "select distinct NEST_ID,  BLOCK_CODE from V_PRD_PART_NEST_LIST order by nest_id"

  private def materialListAllSQL(): String = "select distinct MATERIAL, THICKNESS,NEST_LENGTH,NEST_WIDTH,NEST_ID from V_PRD_PART_NEST_LIST where  NEST_ID like 'MU%' order by MATERIAL,THICKNESS"

  private def materialListByBlocksSQL(blocks: String): String = s"select distinct MATERIAL, THICKNESS,NEST_LENGTH,NEST_WIDTH,NEST_ID from V_PRD_PART_NEST_LIST where  NEST_ID like 'MU%' and BLOCK_CODE in(${blocks})  order by MATERIAL,THICKNESS"

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


  def allPlateNest2(project: String): List[Nest2] = {
    val locks: List[NestLock] = retrieveNestsLocksByProject(project)
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[Nest2]
          val stmt: Statement = connection.createStatement()
          val sql = allPlateNestSQL2()
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            val NESTID: String = Option(rs.getString("NESTID")).getOrElse("")
            val PARTS_WEIGHT: Double = Option(rs.getDouble("PARTS_WEIGHT")).getOrElse(0.0)
            val GROSS_WEIGHT: Double = Option(rs.getDouble("GROSS_WEIGHT")).getOrElse(0.0)
            val CHILDWEIGHT: Double = Option(rs.getDouble("CHILDWEIGHT")).getOrElse(0.0)
            val usage: Double = {
              if (GROSS_WEIGHT > 0.0) {
                (PARTS_WEIGHT + CHILDWEIGHT) / GROSS_WEIGHT * 100.0d
              } else {
                0.0d
              }

            }
            val lock = calculateLockInfo(NESTID, locks)
            buffer += Nest2(
              Option(rs.getInt("OID")).getOrElse(0),
              NESTID,
              Option(rs.getInt("KPL")).getOrElse(0),
              Option(rs.getDouble("LENGTH")).getOrElse(0.0),
              Option(rs.getDouble("WIDTH")).getOrElse(0.0),
              Option(rs.getDouble("THICKNESS")).getOrElse(0.0),
              Option(rs.getString("MAT")).getOrElse(""),
              Option(rs.getDouble("DENSITY")).getOrElse(0.0),
              Option(rs.getString("BLOCKS")).getOrElse(""),
              Option(rs.getInt("NUM_PRT")).getOrElse(0),
              Option(rs.getInt("NUM_EQ_NEST")).getOrElse(0),
              Option(rs.getDouble("AREA")).getOrElse(0.0),
              PARTS_WEIGHT,
              GROSS_WEIGHT,
              usage,
              Option(rs.getString("PARENTNESTID")).getOrElse(""),
              Option(rs.getString("CHILDNESTID")).getOrElse(""),
              Option(rs.getInt("CHILDKPL")).getOrElse(0),
              Option(rs.getDouble("CHILDLENGTH")).getOrElse(0.0),
              Option(rs.getDouble("CHILDWIDTH")).getOrElse(0.0),
              Option(rs.getDouble("CHILDAREAM2")).getOrElse(0.0),
              Option(rs.getDouble("CHILDWEIGHT")).getOrElse(0.0),
              lock._2,
              lock._1
            )
          }
          rs.close()
          stmt.close()
          connection.close()
          rearrangeBlockNests(buffer.toList)
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[Nest2]
        }
      }
      case None => List.empty[Nest2]
    }
  }


  def allPlateNest(project: String): List[Nest] = {
    val nestsBlocks: List[NestIdBlock] = getNestBlock(project)
    val scraps: List[ForanScrap] = genPlateForanScrap(project)
    val locks: List[NestLock] = retrieveNestsLocksByProject(project)
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[Nest]
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(allPlateNestSQL())

          while (rs.next()) {
            val id = Option(rs.getString("ID")).getOrElse("")
            val li: (NestLock, Boolean) = calculateLockInfo(id, locks)
            val L = Option(rs.getInt("NEST_LENGTH")).getOrElse(0)
            val W = Option(rs.getInt("NEST_WIDTH")).getOrElse(0)
            val T: Double = Option(rs.getDouble("THICKNESS")).getOrElse(0.0)
            val RO = Option(rs.getDouble("DENSITY")).getOrElse(0.0)
            val grossW: Int = calculateWeightKG(W, L, T, RO)
            val partsWeight: Double = Option(rs.getDouble("PARTS_WEIGHT")).getOrElse(0)
            val usage: (Double, ForanScrap) = calculateScrap(id, grossW, partsWeight, scraps)
            buffer += Nest(
              id,
              Option(rs.getString("MATERIAL")).getOrElse(""),
              T,
              L,
              W,
              Option(rs.getInt("NUM_EQ_NEST")).getOrElse(0),
              partsWeight,
              RO,
              usage._1,
              nestsBlocks.filter(s => s.nestID.equals(id)).map(_.block).mkString(";"),
              li._2,
              li._1,
              scraps.find(s => s.NESTID.equals(id)) match {
                case Some(value) => value.PARENTNESTID
                case None => ""
              },
              grossW,
              usage._2
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
    val locks: List[NestLock] = retrieveNestsLocksByProject(project)
    val scraps: List[ForanScrap] = genPlateForanScrap(project)


    val nestsBlocks: List[NestIdBlock] = getNestBlock(project)
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[Nest]
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(plateNestByMaterial(mats))
          while (rs.next()) {
            val id = Option(rs.getString("ID")).getOrElse("")
            val li: (NestLock, Boolean) = calculateLockInfo(id, locks)
            val L = Option(rs.getInt("NEST_LENGTH")).getOrElse(0)
            val W = Option(rs.getInt("NEST_WIDTH")).getOrElse(0)
            val T = Option(rs.getDouble("THICKNESS")).getOrElse(0.0)
            val RO = Option(rs.getDouble("DENSITY")).getOrElse(0.0)
            val grossW: Int = calculateWeightKG(W, L, T, RO)
            val partsWeight: Double = Option(rs.getDouble("PARTS_WEIGHT")).getOrElse(0)
            val usage: (Double, ForanScrap) = calculateScrap(id, grossW, partsWeight, scraps)
            buffer += Nest(
              id,
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0.0),
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
              Option(rs.getInt("NUM_EQ_NEST")).getOrElse(0),
              partsWeight,
              Option(rs.getDouble("DENSITY")).getOrElse(0),
              usage._1,
              nestsBlocks.filter(s => s.nestID.equals(id)).map(_.block).mkString(";"),
              li._2,
              li._1,
              scraps.find(s => s.NESTID.equals(id)) match {
                case Some(value) => value.PARENTNESTID
                case None => ""
              },
              grossW,
              usage._2
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
    val locks: List[NestLock] = retrieveNestsLocksByProject(project)
    val scraps: List[ForanScrap] = genPlateForanScrap(project)

    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[Nest]
          val stmt: Statement = connection.createStatement()
          val sql = plateNestByMaterialAndDim(par)
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            val id = Option(rs.getString("ID")).getOrElse("")
            val li: (NestLock, Boolean) = calculateLockInfo(id, locks)
            val L = Option(rs.getInt("NEST_LENGTH")).getOrElse(0)
            val W = Option(rs.getInt("NEST_WIDTH")).getOrElse(0)
            val T = Option(rs.getDouble("THICKNESS")).getOrElse(0.0)
            val RO = Option(rs.getDouble("DENSITY")).getOrElse(0.0)
            val grossW: Int = calculateWeightKG(W, L, T, RO)
            val partsWeight: Double = Option(rs.getDouble("PARTS_WEIGHT")).getOrElse(0)
            val usage: (Double, ForanScrap) = calculateScrap(id, grossW, partsWeight, scraps)
            buffer += Nest(
              id,
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0.0),
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
              Option(rs.getInt("NUM_EQ_NEST")).getOrElse(0),
              partsWeight,
              Option(rs.getDouble("DENSITY")).getOrElse(0.0),
              usage._1,
              nestsBlocks.filter(s => s.nestID.equals(id)).map(_.block).mkString(";"),
              li._2,
              li._1,
              scraps.find(s => s.NESTID.equals(id)) match {
                case Some(value) => value.PARENTNESTID
                case None => ""
              },
              grossW,
              usage._2
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
    val scraps: List[ForanScrap] = genPlateForanScrap(project)

    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[Nest]
          val stmt: Statement = connection.createStatement()
          val sql = plateNestByBlocks(par)
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            val id: String = Option(rs.getString("ID")).getOrElse("")
            val li: (NestLock, Boolean) = calculateLockInfo(id, locks)
            val partsWeight: Double = Option(rs.getDouble("PARTS_WEIGHT")).getOrElse(0)
            val L = Option(rs.getInt("NEST_LENGTH")).getOrElse(0)
            val W = Option(rs.getInt("NEST_WIDTH")).getOrElse(0)
            val T = Option(rs.getDouble("THICKNESS")).getOrElse(0.0)
            val RO = Option(rs.getDouble("DENSITY")).getOrElse(0.0)
            val grossW: Int = calculateWeightKG(W, L, T, RO)
            val usage: (Double, ForanScrap) = calculateScrap(id, grossW, partsWeight, scraps)

            buffer += Nest(
              id,
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0),
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
              Option(rs.getInt("NUM_EQ_NEST")).getOrElse(0),
              partsWeight,
              Option(rs.getDouble("DENSITY")).getOrElse(0),
              usage._1,
              nestsBlocks.filter(s => s.nestID.equals(id)).map(_.block).mkString(";"),
              li._2,
              li._1,
              scraps.find(s => s.NESTID.equals(id)) match {
                case Some(value) => value.PARENTNESTID
                case None => ""
              },
              grossW,
              usage._2
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
    val scraps: List[ForanScrap] = genPlateForanScrap(project)

    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[NestMaterial]
          val stmt: Statement = connection.createStatement()
          val sql = materialListAllSQL()
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            val nestID: String = Option(rs.getString("NEST_ID")).getOrElse("")
            buffer += NestMaterial(
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0),
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
              scraps.find(s => s.NESTID.equals(nestID)) match {
                case Some(value) => value.KPL + " " + value.PARENTNESTID
                case None => ""
              }
            )
          }
          rs.close()
          stmt.close()
          connection.close()
          buffer.toList.distinct
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
    val scraps: List[ForanScrap] = genPlateForanScrap(project)

    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          val buffer = ListBuffer.empty[NestMaterial]
          val stmt: Statement = connection.createStatement()
          val sql = materialListByBlocksSQL(blocks)
          val rs: ResultSet = stmt.executeQuery(sql)
          while (rs.next()) {
            val nestID: String = Option(rs.getString("NEST_ID")).getOrElse("")
            buffer += NestMaterial(
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("THICKNESS")).getOrElse(0),
              Option(rs.getInt("NEST_LENGTH")).getOrElse(0),
              Option(rs.getInt("NEST_WIDTH")).getOrElse(0),
              scraps.find(s => s.NESTID.equals(nestID)) match {
                case Some(value) => value.KPL + " " + value.PARENTNESTID
                case None => ""
              }
            )
          }
          rs.close()
          stmt.close()
          connection.close()
          buffer.toList.distinct
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
          val sql = blocksListSql()
          val rs: ResultSet = stmt.executeQuery(sql)
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

  private def calculateScrap(id: String, grossW: Double, partsWeight: Double, scraps: List[ForanScrap]): (Double, ForanScrap) = {
    val scrap: ForanScrap = {
      scraps.find(s => s.PARENTNESTID.equals(id)) match {
        case Some(value) => value
        case None => ForanScrap()
      }
    }
    val totParts = partsWeight + scrap.WEIGHT
    val usage = totParts / grossW * 100.0d
    (usage, scrap)
  }

  private def rearrangeBlockNests(in: List[Nest2]): List[Nest2] = {

    val arrangesBlocksNames: List[String] = in.filter(s => s.BLOCKS.contains(";")).map(_.BLOCKS).distinct
    val buff = ListBuffer.empty[Nest2]
    in.foreach(card => {
      arrangesBlocksNames.find(s => s.contains(card.BLOCKS) || s.equals(card.BLOCKS)) match {
        case Some(v) =>buff+= Nest2(
          card.OID,
          card.NESTID: String,
          card.KPL,
          card.LENGTH,
          card.WIDTH,
          card.THICKNESS,
          card.MAT,
          card.DENSITY,
          v,
          card.NUMPRT,
          card.NUMEQNEST,
          card.AREA,
          card.PARTSWEIGHT,
          card.GROSSWEIGHT,
          card.USAGE,
          card.PARENTNESTID,
          card.CHILDNESTID,
          card.CHILDKPL,
          card.CHILDLENGTH,
          card.CHILDWIDTH,
          card.CHILDAREAM2,
          card.CHILDWEIGHT,
          card.isLock,
          card.lockInfo
        )
        case None => buff += card
      }
    })
    buff.toList
  }
}
