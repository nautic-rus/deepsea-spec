package local.hull.bill

import local.hull.bill.BillManager.{ForanMaterial, ForanScrap, PlateAnalitic, PlateMaterial, PlateNestBill, ProfileAnalitic, ProfileMaterial, ProfileNestBill, StdPlate}
import local.sql.ConnectionManager

import java.sql.{ResultSet, Statement}
import scala.collection.mutable.ListBuffer

trait BillHelper {

  private def genSTDProfileslSql(): String = "select \n     KSE,\n     DECODE (SECTION,\n                     -1, '',\n                     0, 'FS',\n                     1, 'AS',\n                     2, 'IS',\n                     3, 'TS',\n                     4, 'US',\n                     5, 'BS',\n                     6, 'ST',\n                     7, 'AT',\n                     8, 'OS',\n                     9, 'PS',\n                     10, 'RS',\n                     11, 'MC',\n                     12, 'DB',\n                     13, 'SR',\n                     14, 'HR',\n                     15, 'LI',\n                     16, 'ZL',\n                     17, 'TL',\n                     18, 'AI',\n                     19, 'BL',\n                     20, 'LA',\n                     21, 'TA',\n                     '') as PRF_SECTION,\n     (select CODE from MATERIAL where OID=MATERIAL_OID) as MATERIAL,\n     WEB_HEIGHT as WEB_H,\n     WEB_THICKNESS as WEB_T,\n     FLANGE_HEIGHT as FLANGE_H,\n     FLANGE_THICKNESS as FLANGE_T,\n     STOCK_DEFINED0 as STOCK,\n    0 as PARTS,\n    0 as LENGHT,\n    LENGTH0/1000  as GROWLEN ,\n    0.0 as PARTSWEIGHT,\n    (LENGTH0/1000*area*(select DENSITY from MATERIAL where OID=MATERIAL_OID)/100) as GROWWEIGHT,\n    STOCK_CODE0 as STOCK_CODE\nfrom STD_PROFILE\nwhere SECTION <> 0"

  //not IN(0,1,2,3,6,7,16,17,19,21,24,34,35,36,37) IN( 8,9,10,11,12,13,14,15,18,20,22,23,25,26,31)

  //private def genProfileNestBillSql(): String = "select \n    KSE,\n    KQ,\n    TP,\n    WH,\n    WT,\n    FH,\n    FT,\n    NESTID,\n    NP,\n    NGB,\n    NETLEN,\n    GROLEN,\n    TONETLEN,\n    TOGROLEN,\n    TONETWGT,\n    TOGROWGT,\n    SCRAP,\n    BLOCK,\n    BLOCK_OID,\n    TOTAL_BL_SCRAP,\n    TOTAL_KSE_SCRAP\n    from V_PRD_PRF_MAT"


  //  private def genProfileNestBillSql(): String = "select \n    PPM.KSE,\n    PPM.KQ,\n    PPM.TP,\n    PPM.WH,\n    PPM.WT,\n    PPM.FH,\n    PPM.FT,\n    PPM.NESTID,\n    PPM.NP,\n    PPM.NGB,\n    PPM.NETLEN,\n    PPM.GROLEN,\n    PPM.TONETLEN,\n    PPM.TOGROLEN,\n    PPM.TONETWGT,\n    PPM.TOGROWGT,\n    PPM.SCRAP,\n    PPM.BLOCK,\n    PPM.BLOCK_OID,\n    PPM.TOTAL_BL_SCRAP,\n    PPM.TOTAL_KSE_SCRAP,\n    (select STOCK_DEFINED0 from std_profile where KSE= PPM.KSE) as STOCK\n    from V_PRD_PRF_MAT PPM"
  private def genProfileNestBillSql(): String = "select \n    PPM.KSE,\n    PPM.KQ,\n    PPM.TP,\n    PPM.WH,\n    PPM.WT,\n    PPM.FH,\n    PPM.FT,\n    PPM.NESTID,\n    PPM.NP,\n    PPM.NGB,\n    PPM.NETLEN,\n    PPM.GROLEN,\n    PPM.TONETLEN,\n    PPM.TOGROLEN,\n    PPM.TONETWGT,\n    PPM.TOGROWGT,\n    PPM.SCRAP,\n    PPM.BLOCK,\n    PPM.BLOCK_OID,\n    PPM.TOTAL_BL_SCRAP,\n    PPM.TOTAL_KSE_SCRAP,\n    (select STOCK_DEFINED0 from std_profile where KSE= PPM.KSE) as STOCK,\n    (select STOCK_CODE0 from std_profile where KSE= PPM.KSE) as STOCK_CODE\n    from V_PRD_PRF_MAT PPM"


  //private def genPlateNestBillSql(): String = "select\n    KPL,\n    KQ,\n    L,\n    W,\n    T,\n    NESTID,\n    NP,\n    NGP,\n    TONETWGT,\n    TOGROWGT,\n    SCRAP,\n    BLOCK,\n    BLOCK_OID,\n    TOTAL_BL_SCRAP,\n    TOTAL_KPL_SCRAP,\n    ENCLOS_TYPE,\n    ASPECT_RATIO\n    from\n    V_PRD_PLATE_MAT"
  private def genPlateNestBillSql(): String = "select\n    PL.KPL,\n    PL.KQ,\n    PL.L,\n    PL.W,\n    PL.T,\n    PL.NESTID,\n    PL.NP,\n    PL.NGP,\n    PL.TONETWGT,\n    PL.TOGROWGT,\n    PL.SCRAP,\n    PL.BLOCK,\n    PL.BLOCK_OID,\n    PL.TOTAL_BL_SCRAP,\n    PL.TOTAL_KPL_SCRAP,\n    PL.ENCLOS_TYPE,\n    PL.ASPECT_RATIO,\n    STD.STOCK\n    from\n    V_PRD_PLATE_MAT PL, STD_PLATE STD\n    where PL.KPL=STD.KPL"
  // private def genTotalPlatesMateriallSql(): String = "select count(*) as COUNT , \nsum(WEIGHT) as WEIGHT,  THICKNESS, MAT \nfrom(\nselect \n PEP.WEIGHT,\n (SELECT THICKNESS AS THK FROM prd_plate_att\n WHERE prd_part_oid=P.OID\n UNION ALL\n SELECT THICKNESS AS THK FROM shdk_std_plate\n WHERE prd_part_oid=P.OID\n UNION ALL\n SELECT THICKNESS*1000 AS THK FROM inp_plate_att_db\n WHERE inp_part_oid=P.OID\n UNION ALL\n SELECT THICKNESS AS THK FROM as_std_part_plate\n WHERE oid=P.OID) as THICKNESS ,\n  (\n select code as MAT from MATERIAL where oid=(SELECT MATERIAL_OID FROM prd_plate_att WHERE prd_part_oid=P.OID)\n UNION ALL\n select code as MAT from MATERIAL where oid=(SELECT MATERIAL_OID FROM shdk_std_plate WHERE prd_part_oid=P.OID)\n UNION ALL\n select code as MAT from MATERIAL where oid=(SELECT MATERIAL_OID FROM inp_plate_att_db WHERE inp_part_oid=P.OID)\n UNION ALL\n SELECT MATQ AS MAT FROM as_std_part_plate\n WHERE oid=P.OID) as MAT \nfrom prd_part P,PRD_EXPL_PART PEP\nwhere\nPART_TYPE IN( 8,9,10,11,12,13,14,15,18,20,22,23,25,26,31)\nAND PEP.PRD_PART_OID=P.OID\nUNION ALL\nselect\nWEIGHT,\n(select WEB_THICKNESS from STD_PROFILE where KSE=GKSE) as THICKNESS,\n(select code from MATERIAL where oid=(select MATERIAL_OID from STD_PROFILE where KSE=GKSE)) as MAT \nfrom(\nselect \nsum(WEIGHT) as WEIGHT,\nKSE as GKSE\nfrom(\nselect \nPEP.WEIGHT,\n (SELECT STD.kse AS KSE\n FROM STD_PROFILE STD, PRD_PROFILE PRF\n WHERE STD.OID=PRF.STD_PROFILE_OID AND PRF.PRD_PART_OID=P.OID and STD.SECTION=0\n UNION ALL\n SELECT STD.kse AS KSE \n FROM STD_PROFILE STD, INP_PROFILE_ATT_DB PRF\n WHERE STD.OID=PRF.STD_PROFILE_OID AND PRF.INP_PART_OID=P.OID and STD.SECTION=0\n UNION ALL\n SELECT STD.kse AS KSE\n FROM STD_PROFILE STD, INP_LC_ATT_DB PRF\n WHERE STD.OID=PRF.STD_SECTION_OID AND PRF.INP_PART_OID=P.OID and STD.SECTION=0\n  UNION ALL\n SELECT STD.kse AS KSE\n FROM STD_PROFILE STD, AS_STD_PART_PROF PRF\n WHERE STD.KSE=PRF.KSE AND PRF.OID=P.OID  and STD.SECTION=0\n) AS KSE\n\nfrom prd_part P,PRD_EXPL_PART PEP\nwhere\nPART_TYPE =17\nAND PEP.PRD_PART_OID=P.OID\n)\ngroup by KSE\n)\n)\nwhere THICKNESS is not null and  MAT is not null and THICKNESS <>0\ngroup by THICKNESS, MAT  \norder by THICKNESS, MAT "

  //private def genTotalPlatesMateriallSql(): String = "select sum(COUNT)   as COUNT, \nsum(WEIGHT) as WEIGHT,  THICKNESS, MAT \n\nfrom\n(\n\nselect count(*) as COUNT , \nsum(WEIGHT) as WEIGHT,  THICKNESS, MAT \nfrom(\nselect \n PEP.WEIGHT,\n (SELECT THICKNESS AS THK FROM prd_plate_att\n WHERE prd_part_oid=P.OID\n UNION ALL\n SELECT THICKNESS AS THK FROM shdk_std_plate\n WHERE prd_part_oid=P.OID\n UNION ALL\n SELECT THICKNESS*1000 AS THK FROM inp_plate_att_db\n WHERE inp_part_oid=P.OID\n UNION ALL\n SELECT THICKNESS AS THK FROM as_std_part_plate\n WHERE oid=P.OID) as THICKNESS ,\n  (\n select code as MAT from MATERIAL where oid=(SELECT MATERIAL_OID FROM prd_plate_att WHERE prd_part_oid=P.OID)\n UNION ALL\n select code as MAT from MATERIAL where oid=(SELECT MATERIAL_OID FROM shdk_std_plate WHERE prd_part_oid=P.OID)\n UNION ALL\n select code as MAT from MATERIAL where oid=(SELECT MATERIAL_OID FROM inp_plate_att_db WHERE inp_part_oid=P.OID)\n UNION ALL\n SELECT MATQ AS MAT FROM as_std_part_plate\n WHERE oid=P.OID) as MAT \nfrom prd_part P,PRD_EXPL_PART PEP\nwhere\nPART_TYPE IN( 8,9,10,11,12,13,14,15,18,20,22,23,25,26,31,16)\nAND PEP.PRD_PART_OID=P.OID\n)\ngroup by THICKNESS, MAT  \n UNION ALL\nselect\nCOUNT,\nWEIGHT,\n(select WEB_THICKNESS from STD_PROFILE where KSE=GKSE) as THICKNESS,\n(select code from MATERIAL where oid=(select MATERIAL_OID from STD_PROFILE where KSE=GKSE)) as MAT \nfrom(\nselect\ncount(*) as COUNT,\nsum(WEIGHT)as WEIGHT,\nGKSE\nfrom\n(\nselect \nPEP.WEIGHT,\n (SELECT STD.kse AS KSE\n FROM STD_PROFILE STD, PRD_PROFILE PRF\n WHERE STD.OID=PRF.STD_PROFILE_OID AND PRF.PRD_PART_OID=P.OID and STD.SECTION=0\n UNION ALL\n SELECT STD.kse AS KSE \n FROM STD_PROFILE STD, INP_PROFILE_ATT_DB PRF\n WHERE STD.OID=PRF.STD_PROFILE_OID AND PRF.INP_PART_OID=P.OID and STD.SECTION=0\n UNION ALL\n SELECT STD.kse AS KSE\n FROM STD_PROFILE STD, INP_LC_ATT_DB PRF\n WHERE STD.OID=PRF.STD_SECTION_OID AND PRF.INP_PART_OID=P.OID and STD.SECTION=0\n  UNION ALL\n SELECT STD.kse AS KSE\n FROM STD_PROFILE STD, AS_STD_PART_PROF PRF\n WHERE STD.KSE=PRF.KSE AND PRF.OID=P.OID  and STD.SECTION=0\n) AS GKSE\n\nfrom prd_part P,PRD_EXPL_PART PEP\nwhere\nPART_TYPE in(17,19,24,21)\nAND PEP.PRD_PART_OID=P.OID\n)\nWhere GKSE is not null\ngroup by GKSE\n)\n)\nwhere THICKNESS is not null group by THICKNESS, MAT  \norder by THICKNESS, MAT  "
  private def genTotalPlatesMateriallSql(): String = "select sum(COUNT)   as COUNT, \nsum(WEIGHT) as WEIGHT,  THICKNESS, MAT \n\nfrom\n(\n\nselect count(*) as COUNT , \nsum(WEIGHT) as WEIGHT,  THICKNESS, MAT \nfrom(\nselect \n PEP.WEIGHT,\n (SELECT THICKNESS AS THK FROM prd_plate_att\n WHERE prd_part_oid=P.OID\n UNION ALL\n SELECT THICKNESS AS THK FROM shdk_std_plate\n WHERE prd_part_oid=P.OID\n UNION ALL\n SELECT THICKNESS*1000 AS THK FROM inp_plate_att_db\n WHERE inp_part_oid=P.OID\n UNION ALL\n SELECT THICKNESS AS THK FROM as_std_part_plate\n WHERE oid=P.OID) as THICKNESS ,\n  (\n select code as MAT from MATERIAL where oid=(SELECT MATERIAL_OID FROM prd_plate_att WHERE prd_part_oid=P.OID)\n UNION ALL\n select code as MAT from MATERIAL where oid=(SELECT MATERIAL_OID FROM shdk_std_plate WHERE prd_part_oid=P.OID)\n UNION ALL\n select code as MAT from MATERIAL where oid=(SELECT MATERIAL_OID FROM inp_plate_att_db WHERE inp_part_oid=P.OID)\n UNION ALL\n SELECT MATQ AS MAT FROM as_std_part_plate\n WHERE oid=P.OID) as MAT \nfrom prd_part P,PRD_EXPL_PART PEP\nwhere\nP.PART_TYPE IN( 8,9,10,11,12,13,14,15,18,20,22,23,25,26,31,16) \nAND P.BLOCK_OID not in (select OID from BLOCK where code like 'L%')\nAND PEP.PRD_PART_OID=P.OID\n)\ngroup by THICKNESS, MAT  \n UNION ALL\nselect\nCOUNT,\nWEIGHT,\n(select WEB_THICKNESS from STD_PROFILE where KSE=GKSE) as THICKNESS,\n(select code from MATERIAL where oid=(select MATERIAL_OID from STD_PROFILE where KSE=GKSE)) as MAT \nfrom(\nselect\ncount(*) as COUNT,\nsum(WEIGHT)as WEIGHT,\nGKSE\nfrom\n(\nselect \nPEP.WEIGHT,\n (SELECT STD.kse AS KSE\n FROM STD_PROFILE STD, PRD_PROFILE PRF\n WHERE STD.OID=PRF.STD_PROFILE_OID AND PRF.PRD_PART_OID=P.OID and STD.SECTION=0\n UNION ALL\n SELECT STD.kse AS KSE \n FROM STD_PROFILE STD, INP_PROFILE_ATT_DB PRF\n WHERE STD.OID=PRF.STD_PROFILE_OID AND PRF.INP_PART_OID=P.OID and STD.SECTION=0\n UNION ALL\n SELECT STD.kse AS KSE\n FROM STD_PROFILE STD, INP_LC_ATT_DB PRF\n WHERE STD.OID=PRF.STD_SECTION_OID AND PRF.INP_PART_OID=P.OID and STD.SECTION=0\n  UNION ALL\n SELECT STD.kse AS KSE\n FROM STD_PROFILE STD, AS_STD_PART_PROF PRF\n WHERE STD.KSE=PRF.KSE AND PRF.OID=P.OID  and STD.SECTION=0\n) AS GKSE\n\nfrom prd_part P,PRD_EXPL_PART PEP\nwhere\nP.PART_TYPE in(17,19,24,21)\nAND P.BLOCK_OID not in (select OID from BLOCK where code like 'L%')\nAND PEP.PRD_PART_OID=P.OID\n)\nWhere GKSE is not null\ngroup by GKSE\n)\n)\nwhere THICKNESS is not null group by THICKNESS, MAT  \norder by THICKNESS, MAT  "

  //private def genTotalProfilesMateriallSqlOld(): String = "select\n    KSE,\n    PRF_SECTION,\n    MATERIAL,\n    WEB_H,\n    WEB_T,\n    FLANGE_H,\n    FLANGE_T,\n    sum(PART_NUM) as PARTS,\n    sum(TOTAL_LEN) as LENGHT\nfrom\nV_TOTAL_PROFILE_BILL\nwhere PRF_SECTION <>'FS'   group by KSE,\n    PRF_SECTION,\n    MATERIAL,\n    WEB_H,\n    WEB_T,\n    FLANGE_H,\n    FLANGE_T\n    order by KSE"
  //private def genTotalProfilesMateriallSql(): String ="select \n     KSE,\n     PRF_SECTION,\n     MATERIAL,\n     WEB_H,\n     WEB_T,\n     FLANGE_H,\n     FLANGE_T,\n    PARTS,\n    LENGHT,\n    (LENGHT*area*density/100) as PARTSWEIGHT\nfrom(\nselect\n     VPB.KSE,\n     VPB.PRF_SECTION,\n     VPB.MATERIAL,\n     VPB.WEB_H,\n     VPB.WEB_T,\n     VPB.FLANGE_H,\n     VPB.FLANGE_T,\n     s.area,\n     m.density,\n    sum( VPB.PART_NUM) as PARTS,\n    sum( VPB.TOTAL_LEN) as LENGHT\nfrom\nV_TOTAL_PROFILE_BILL VPB, std_profile s, material m\nwhere  \ns.KSE=VPB.KSE AND m.CODE=VPB.MATERIAL AND\n     VPB.PRF_SECTION <>'FS'   group by  VPB.KSE,\n     VPB.PRF_SECTION,\n     VPB.MATERIAL,\n     VPB.WEB_H,\n     VPB.WEB_T,\n     VPB.FLANGE_H,\n     VPB.FLANGE_T,\n     s.area,\n     m.density\n    order by  VPB.KSE)"
  //private def genTotalProfilesMateriallSql(): String = "select \n     KSE,\n     PRF_SECTION,\n     MATERIAL,\n     WEB_H,\n     WEB_T,\n     FLANGE_H,\n     FLANGE_T,\n    PARTS,\n    LENGHT,\n    (LENGHT*area*density/100) as PARTSWEIGHT\nfrom(\nselect\n     VPB.KSE,\n     VPB.PRF_SECTION,\n     VPB.MATERIAL,\n     VPB.WEB_H,\n     VPB.WEB_T,\n     VPB.FLANGE_H,\n     VPB.FLANGE_T,\n     s.area,\n     m.density,\n    sum( VPB.PART_NUM) as PARTS,\n    sum( VPB.TOTAL_LEN) as LENGHT\nfrom\n(      SELECT STD_KSE as KSE,\n             DECODE (STD_SECTION,\n                     -1, '',\n                     0, 'FS',\n                     1, 'AS',\n                     2, 'IS',\n                     3, 'TS',\n                     4, 'US',\n                     5, 'BS',\n                     6, 'ST',\n                     7, 'AT',\n                     8, 'OS',\n                     9, 'PS',\n                     10, 'RS',\n                     11, 'MC',\n                     12, 'DB',\n                     13, 'SR',\n                     14, 'HR',\n                     15, 'LI',\n                     16, 'ZL',\n                     17, 'TL',\n                     18, 'AI',\n                     19, 'BL',\n                     20, 'LA',\n                     21, 'TA',\n                     '') as PRF_SECTION,\n             MAT_CODE as MATERIAL,\n             STD_WEB_HEIGHT as WEB_H,\n             STD_WEB_THICKNESS as WEB_T,\n             STD_FLANGE_HEIGHT as FLANGE_H,\n             STD_FLANGE_THICKNESS as FLANGE_T,\n             COUNT (PART_OID) as PART_NUM,\n             SUM (PRF_LENGTH) as TOTAL_LEN\n        FROM V_RPT_ALL_PROF_DATA\n         where BL_OID not in (select OID from BLOCK where code like 'L%')\n    GROUP BY STD_OID,\n             STD_KSE,\n             STD_SECTION,\n             STD_WEB_HEIGHT,\n             STD_WEB_THICKNESS,\n             STD_FLANGE_HEIGHT,\n             STD_FLANGE_THICKNESS,\n             MAT_CODE) VPB, std_profile s, material m\nwhere  \ns.KSE=VPB.KSE AND m.CODE=VPB.MATERIAL AND\n     VPB.PRF_SECTION <>'FS'   group by  VPB.KSE,\n     VPB.PRF_SECTION,\n     VPB.MATERIAL,\n     VPB.WEB_H,\n     VPB.WEB_T,\n     VPB.FLANGE_H,\n     VPB.FLANGE_T,\n     s.area,\n     m.density\n    order by  VPB.KSE)"
  //private def genTotalProfilesMateriallSql(): String = "select \n     KSE,\n     PRF_SECTION,\n     MATERIAL,\n     WEB_H,\n     WEB_T,\n     FLANGE_H,\n     FLANGE_T,\n     STOCK,\n    PARTS,\n    LENGHT,\n    (LENGHT*area*density/100) as PARTSWEIGHT\nfrom(\n\nselect\n     VPB.KSE,\n     VPB.PRF_SECTION,\n     VPB.MATERIAL,\n     VPB.WEB_H,\n     VPB.WEB_T,\n     VPB.FLANGE_H,\n     VPB.FLANGE_T,\n     s.area,\n     m.density,\n     s.STOCK_DEFINED0 as STOCK,\n    sum( VPB.PART_NUM) as PARTS,\n    sum( VPB.TOTAL_LEN) as LENGHT\nfrom\n(      SELECT STD_KSE as KSE,\n             DECODE (STD_SECTION,\n                     -1, '',\n                     0, 'FS',\n                     1, 'AS',\n                     2, 'IS',\n                     3, 'TS',\n                     4, 'US',\n                     5, 'BS',\n                     6, 'ST',\n                     7, 'AT',\n                     8, 'OS',\n                     9, 'PS',\n                     10, 'RS',\n                     11, 'MC',\n                     12, 'DB',\n                     13, 'SR',\n                     14, 'HR',\n                     15, 'LI',\n                     16, 'ZL',\n                     17, 'TL',\n                     18, 'AI',\n                     19, 'BL',\n                     20, 'LA',\n                     21, 'TA',\n                     '') as PRF_SECTION,\n             MAT_CODE as MATERIAL,\n             STD_WEB_HEIGHT as WEB_H,\n             STD_WEB_THICKNESS as WEB_T,\n             STD_FLANGE_HEIGHT as FLANGE_H,\n             STD_FLANGE_THICKNESS as FLANGE_T,\n             COUNT (PART_OID) as PART_NUM,\n             SUM (PRF_LENGTH) as TOTAL_LEN\n           \n        FROM V_RPT_ALL_PROF_DATA\n         where BL_OID not in (select OID from BLOCK where code like 'L%')\n    GROUP BY STD_OID,\n             STD_KSE,\n             STD_SECTION,\n             STD_WEB_HEIGHT,\n             STD_WEB_THICKNESS,\n             STD_FLANGE_HEIGHT,\n             STD_FLANGE_THICKNESS,\n             MAT_CODE) VPB, std_profile s, material m\nwhere  \ns.KSE=VPB.KSE AND m.CODE=VPB.MATERIAL AND\n     VPB.PRF_SECTION <>'FS'   \n     \n     group by  \n     VPB.KSE,\n     VPB.PRF_SECTION,\n     VPB.MATERIAL,\n     VPB.WEB_H,\n     VPB.WEB_T,\n     VPB.FLANGE_H,\n     VPB.FLANGE_T,\n     s.area,\n     m.density,\n     s.STOCK_DEFINED0\n    order by  VPB.KSE\n    )"
  private def genTotalProfilesMateriallSql(): String = "select \n     KSE,\n     PRF_SECTION,\n     MATERIAL,\n     WEB_H,\n     WEB_T,\n     FLANGE_H,\n     FLANGE_T,\n     STOCK,\n    PARTS,\n    LENGHT,\n    GROWLEN/1000  as GROWLEN ,\n    (LENGHT*area*density/100) as PARTSWEIGHT,\n    (GROWLEN/1000*area*density/100) as GROWWEIGHT\n    \nfrom(\n\nselect\n     VPB.KSE,\n     VPB.PRF_SECTION,\n     VPB.MATERIAL,\n     VPB.WEB_H,\n     VPB.WEB_T,\n     VPB.FLANGE_H,\n     VPB.FLANGE_T,\n     s.area,\n     m.density,\n     s.STOCK_DEFINED0 as STOCK,\n     s.LENGTH0 as GROWLEN,\n    sum( VPB.PART_NUM) as PARTS,\n    sum( VPB.TOTAL_LEN) as LENGHT\nfrom\n(      SELECT STD_KSE as KSE,\n             DECODE (STD_SECTION,\n                     -1, '',\n                     0, 'FS',\n                     1, 'AS',\n                     2, 'IS',\n                     3, 'TS',\n                     4, 'US',\n                     5, 'BS',\n                     6, 'ST',\n                     7, 'AT',\n                     8, 'OS',\n                     9, 'PS',\n                     10, 'RS',\n                     11, 'MC',\n                     12, 'DB',\n                     13, 'SR',\n                     14, 'HR',\n                     15, 'LI',\n                     16, 'ZL',\n                     17, 'TL',\n                     18, 'AI',\n                     19, 'BL',\n                     20, 'LA',\n                     21, 'TA',\n                     '') as PRF_SECTION,\n             MAT_CODE as MATERIAL,\n             STD_WEB_HEIGHT as WEB_H,\n             STD_WEB_THICKNESS as WEB_T,\n             STD_FLANGE_HEIGHT as FLANGE_H,\n             STD_FLANGE_THICKNESS as FLANGE_T,\n             COUNT (PART_OID) as PART_NUM,\n             SUM (PRF_LENGTH) as TOTAL_LEN\n           \n        FROM V_RPT_ALL_PROF_DATA\n         where BL_OID not in (select OID from BLOCK where code like 'L%')\n    GROUP BY STD_OID,\n             STD_KSE,\n             STD_SECTION,\n             STD_WEB_HEIGHT,\n             STD_WEB_THICKNESS,\n             STD_FLANGE_HEIGHT,\n             STD_FLANGE_THICKNESS,\n             MAT_CODE) VPB, std_profile s, material m\nwhere  \ns.KSE=VPB.KSE AND m.CODE=VPB.MATERIAL AND\n     VPB.PRF_SECTION <>'FS'   \n     \n     group by  \n     VPB.KSE,\n     VPB.PRF_SECTION,\n     VPB.MATERIAL,\n     VPB.WEB_H,\n     VPB.WEB_T,\n     VPB.FLANGE_H,\n     VPB.FLANGE_T,\n     s.area,\n     m.density,\n     s.STOCK_DEFINED0,\n     s.LENGTH0\n    order by  VPB.KSE\n    )"

  private def genForanScrapSQL() = "select \nENCL.OID,\nENCL.STD_PLATE_OID as STDPLATEOID,\nSTD.KPL,\nSTD2.KPL as PARENTKPL,\nGET_NEST_ID( \n(select OID from PRD_NEST_GEN where COD_PLT=(select OID from STD_PLATE where KPL=STD.KPL)),\n(select ID_SEQ_BLOCK from PRD_NEST_GEN where COD_PLT=(select OID from STD_PLATE where KPL=STD.KPL))\n) as NESTID,\nGET_NEST_ID(ENCL.FROM_NEST_OID, N.ID_SEQ_BLOCK) PARENTNESTID,\nENCL.TYPE,\nENCL.SLENGTH,\nENCL.SWIDTH,\nSTD.THICKNESS,\nMAT.DENSITY,\nENCL.AREA_M2 as AREAM2,\n(STD.THICKNESS/10*ENCL.AREA_M2*10000*MAT.DENSITY)/1000 as WEIGHT\n\nfrom PRD_STD_PLT_ENCLOS ENCL, STD_PLATE STD, MATERIAL MAT, PRD_NEST_GEN N, STD_PLATE STD2\nwhere \nENCL.STD_PROFILE_OID is NULL AND\nSTD.OID=ENCL.STD_PLATE_OID AND\nMAT.OID=STD.MATERIAL_OID AND\nN.OID=ENCL.FROM_NEST_OID AND\nSTD2.OID=N.COD_PLT"

  def genProfileNestBill(project: String): List[ProfileNestBill] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = genProfileNestBillSql()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[ProfileNestBill]
          while (rs.next()) {
            ret += ProfileNestBill(
              Option(rs.getInt("KSE")).getOrElse(0),
              Option(rs.getString("KQ")).getOrElse(""),
              Option(rs.getString("TP")).getOrElse(""),
              Option(rs.getDouble("WH")).getOrElse(0.0),
              Option(rs.getDouble("WT")).getOrElse(0.0),
              Option(rs.getDouble("FH")).getOrElse(0.0),
              Option(rs.getDouble("FT")).getOrElse(0.0),
              Option(rs.getString("NESTID")).getOrElse(""),
              Option(rs.getInt("NP")).getOrElse(0),
              Option(rs.getInt("NGB")).getOrElse(0),
              Option(rs.getDouble("NETLEN")).getOrElse(0.0),
              Option(rs.getDouble("GROLEN")).getOrElse(0.0),
              Option(rs.getDouble("TONETLEN")).getOrElse(0.0),
              Option(rs.getDouble("TOGROLEN")).getOrElse(0.0),
              Option(rs.getDouble("TONETWGT")).getOrElse(0.0),
              Option(rs.getDouble("TOGROWGT")).getOrElse(0.0),
              Option(rs.getDouble("SCRAP")).getOrElse(0.0),
              Option(rs.getString("BLOCK")).getOrElse(""),
              Option(rs.getInt("BLOCK_OID")).getOrElse(0),
              Option(rs.getDouble("TOTAL_BL_SCRAP")).getOrElse(0.0),
              Option(rs.getDouble("TOTAL_KSE_SCRAP")).getOrElse(0.0),
              Math.ceil(Option(rs.getDouble("STOCK")).getOrElse(0.0)).toInt,
              Option(rs.getString("STOCK_CODE")).getOrElse("")
            )
          }

          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[ProfileNestBill]
        }
      }
      case None => List.empty[ProfileNestBill]
    }
  }

  def genPlateNestBill(project: String): List[PlateNestBill] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = genPlateNestBillSql()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[PlateNestBill]
          while (rs.next()) {
            ret += PlateNestBill(
              Option(rs.getInt("KPL")).getOrElse(0),
              Option(rs.getString("KQ")).getOrElse(""),
              Option(rs.getDouble("L")).getOrElse(0.0),
              Option(rs.getDouble("W")).getOrElse(0.0),
              Option(rs.getDouble("T")).getOrElse(0.0),
              Option(rs.getString("NESTID")).getOrElse(""),
              Option(rs.getInt("NP")).getOrElse(0),
              Option(rs.getInt("NGP")).getOrElse(0),
              Option(rs.getDouble("TONETWGT")).getOrElse(0.0),
              Option(rs.getDouble("TOGROWGT")).getOrElse(0.0),
              Option(rs.getDouble("SCRAP")).getOrElse(0.0),
              Option(rs.getString("BLOCK")).getOrElse(""),
              Option(rs.getInt("BLOCK_OID")).getOrElse(0),
              Option(rs.getDouble("TOTAL_BL_SCRAP")).getOrElse(0.0),
              Option(rs.getDouble("TOTAL_KPL_SCRAP")).getOrElse(0.0),
              Option(rs.getInt("ENCLOS_TYPE")).getOrElse(0),
              Option(rs.getInt("ASPECT_RATIO")).getOrElse(0),
              Option(rs.getInt("STOCK")).getOrElse(0)
            )
          }

          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[PlateNestBill]
        }
      }
      case None => List.empty[PlateNestBill]
    }
  }

  def genPlateForanScrap(project: String): List[ForanScrap] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = genForanScrapSQL()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[ForanScrap]
          while (rs.next()) {
            ret += ForanScrap(
              Option(rs.getInt("OID")).getOrElse(0),
              Option(rs.getInt("STDPLATEOID")).getOrElse(0),
              Option(rs.getInt("KPL")).getOrElse(0),
              Option(rs.getInt("PARENTKPL")).getOrElse(0),
              Option(rs.getString("NESTID")).getOrElse(""),
              Option(rs.getString("PARENTNESTID")).getOrElse(""),
              Option(rs.getInt("TYPE")).getOrElse(0),
              Option(rs.getDouble("SLENGTH")).getOrElse(0.0),
              Option(rs.getDouble("SWIDTH")).getOrElse(0.0),
              Option(rs.getDouble("THICKNESS")).getOrElse(0.0),
              Option(rs.getDouble("DENSITY")).getOrElse(0.0),
              Option(rs.getDouble("AREAM2")).getOrElse(0.0),
              Option(rs.getDouble("WEIGHT")).getOrElse(0.0)
            )
          }

          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[ForanScrap]
        }
      }
      case None => List.empty[ForanScrap]
    }
  }

  def genTotPlates(project: String): List[PlateMaterial] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = genTotalPlatesMateriallSql()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[PlateMaterial]
          while (rs.next()) {
            ret += PlateMaterial(
              Option(rs.getInt("COUNT")).getOrElse(0),
              Option(rs.getDouble("WEIGHT")).getOrElse(0.0),
              Option(rs.getDouble("THICKNESS")).getOrElse(0.0),
              Option(rs.getString("MAT")).getOrElse("")
            )
          }
          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[PlateMaterial]
        }
      }
      case None => List.empty[PlateMaterial]
    }
  }

  def genSTDprofiles(project: String): List[ProfileMaterial] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = genSTDProfileslSql()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[ProfileMaterial]
          while (rs.next()) {
            ret += ProfileMaterial(
              Option(rs.getInt("KSE")).getOrElse(0),
              Option(rs.getString("PRF_SECTION")).getOrElse(""),
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("WEB_H")).getOrElse(0.0),
              Option(rs.getDouble("WEB_T")).getOrElse(0.0),
              Option(rs.getDouble("FLANGE_H")).getOrElse(0.0),
              Option(rs.getDouble("FLANGE_T")).getOrElse(0.0),
              Option(rs.getInt("PARTS")).getOrElse(0),
              Option(rs.getDouble("LENGHT")).getOrElse(0.0),
              Option(rs.getDouble("PARTSWEIGHT")).getOrElse(0.0),
              Math.ceil(Option(rs.getDouble("STOCK")).getOrElse(0.0)).toInt,
              Option(rs.getDouble("GROWLEN")).getOrElse(0.0),
              Option(rs.getDouble("GROWWEIGHT")).getOrElse(0.0),
              Option(rs.getString("STOCK_CODE")).getOrElse("")
            )
          }
          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[ProfileMaterial]
        }
      }
      case None => List.empty[ProfileMaterial]
    }
  }

  def genTotProfiles(project: String): List[ProfileMaterial] = {
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val sql = genTotalProfilesMateriallSql()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[ProfileMaterial]
          while (rs.next()) {
            ret += ProfileMaterial(
              Option(rs.getInt("KSE")).getOrElse(0),
              Option(rs.getString("PRF_SECTION")).getOrElse(""),
              Option(rs.getString("MATERIAL")).getOrElse(""),
              Option(rs.getDouble("WEB_H")).getOrElse(0.0),
              Option(rs.getDouble("WEB_T")).getOrElse(0.0),
              Option(rs.getDouble("FLANGE_H")).getOrElse(0.0),
              Option(rs.getDouble("FLANGE_T")).getOrElse(0.0),
              Option(rs.getInt("PARTS")).getOrElse(0),
              Option(rs.getDouble("LENGHT")).getOrElse(0.0),
              Option(rs.getDouble("PARTSWEIGHT")).getOrElse(0.0),
              Math.ceil(Option(rs.getDouble("STOCK")).getOrElse(0.0)).toInt,
              Option(rs.getDouble("GROWLEN")).getOrElse(0.0),
              Option(rs.getDouble("GROWWEIGHT")).getOrElse(0.0)
            )
          }
          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[ProfileMaterial]
        }
      }
      case None => List.empty[ProfileMaterial]
    }
  }

  def genScantling(WH: Double, WT: Double = 0.0, FH: Double = 0.0, FT: Double = 0.0): String = {
    var s = ""
    if (!doubTo2Str(WH).equals("0")) s += doubTo2Str(WH)
    if (!doubTo2Str(WT).equals("0")) s += "x" + doubTo2Str(WT)
    if (!doubTo2Str(FH).equals("0")) s += "x" + doubTo2Str(FH)
    if (!doubTo2Str(FT).equals("0")) s += "x" + doubTo2Str(FT)
    s
  }

  def doubTo2Str(in: Double): (String) = {
    val arr = in.toString.split("\\.")
    val intVal = arr.head
    val doubleValue = arr(1).head.toString
    if (doubleValue.equals("0")) {
      intVal
    } else {
      intVal + "." + doubleValue
    }
  }

  def genForanMaterials(project: String): List[ForanMaterial] = {
    val sql = "select OID,CODE,DESCRIPTION,DENSITY from MATERIAL"
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[ForanMaterial]
          while (rs.next()) {
            ret += ForanMaterial(
              Option(rs.getInt("OID")).getOrElse(0),
              Option(rs.getString("CODE")).getOrElse(""),
              Option(rs.getString("DESCRIPTION")).getOrElse(""),
              Option(rs.getDouble("DENSITY")).getOrElse(0.0)
            )
          }
          rs.close()
          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[ForanMaterial]
        }
      }
      case None => List.empty[ForanMaterial]
    }
  }

  def genForanStdPlates(project: String): List[StdPlate] = {
    val sql = "select OID,KPL,MATERIAL_OID,LENGTH,WIDTH,THICKNESS,STORAGE_CODE,STOCK from STD_PLATE "
    ConnectionManager.connectionByProject(project) match {
      case Some(connection) => {
        try {
          connection.setAutoCommit(false)
          val stmt: Statement = connection.createStatement()
          val rs: ResultSet = stmt.executeQuery(sql)
          val ret = ListBuffer.empty[StdPlate]
          while (rs.next()) {
            ret += StdPlate(
              Option(rs.getInt("OID")).getOrElse(0),
              Option(rs.getInt("KPL")).getOrElse(0),
              Option(rs.getInt("MATERIAL_OID")).getOrElse(0),
              Option(rs.getDouble("LENGTH")).getOrElse(0.0),
              Option(rs.getDouble("WIDTH")).getOrElse(0.0),
              Option(rs.getDouble("THICKNESS")).getOrElse(0.0),
              Option(rs.getString("STORAGE_CODE")).getOrElse(""),
              Option(rs.getInt("STOCK")).getOrElse(0),
              Option(rs.getDouble("LENGTH")).getOrElse(0.0) * Option(rs.getDouble("WIDTH")).getOrElse(0.0)
            )
          }
          rs.close()
          stmt.close()
          connection.close()
          ret.toList
        }
        catch {
          case _: Throwable =>
            connection.close()
            List.empty[StdPlate]
        }
      }
      case None => List.empty[StdPlate]
    }
  }

}
