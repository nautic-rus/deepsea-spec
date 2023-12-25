select P.PART_OID as PART_OID,
       P.NUM_PART_NEST as QTY,
       P.PART_CODE,
       P.SYMMETRY,
       P.PART_TYPE,
       P.BLOCK_CODE,
       P.BLOCK_CODE as DESCRIPTION,
       P.NUM_EQ_PART,
       P.PART_DESC,
       P.ELEM_TYPE,
       P.MATERIAL,
       P.LENGTH,
       P.WIDTH,
       P.THICKNESS,
       P.WEIGHT_UNIT,
       P.TOTAL_WEIGHT,
       P.NEST_ID,
       P.NEST_LENGTH,
       P.NEST_WIDTH,
       P.NUM_EQ_NEST,
       P.WH,
       P.WT,
       P.FH,
       P.FT ,
       RPT_GET_STR_GROUP(P.PART_OID) as STRGROUP ,
       P.STOCK as STOCK_CODE

from
    (
        select
            P.OID as PART_OID,
            PE.OID as PEOID,
            P.CODE  as PART_CODE,
            DECODE (PE.SYMMETRY,  0,'P', 1,'S', 2,'C', 3,'') as SYMMETRY,
            P.PART_TYPE,
            B.CODE as BLOCK_CODE,
            RPT_COPY_COUNT (P.OID) as NUM_EQ_PART,
            SUBSTR (GET_PART_DESC (P.OID, 1), 1, 200) as PART_DESC ,
            SUBSTR (GET_SECT_TP (prf.std_profile_oid), 1, 2) as ELEM_TYPE,
            SUBSTR (GET_MATERIAL_CODE (PRF.STD_PROFILE_OID, 2), 1, 12) as MATERIAL,
            RPT_GET_PART_LENGTH (P.OID) * 1000 as LENGTH,
            STD.WEB_HEIGHT as WIDTH,
            STD.WEB_THICKNESS as THICKNESS,
            RPT_GET_PART_WEIGHT (P.OID) as WEIGHT_UNIT,
            RPT_GET_PART_WEIGHT (P.OID) * RPT_COPY_COUNT (P.OID) as TOTAL_WEIGHT,
            SUBSTR (GET_NEST_ID (GET_PRD_EXPL_NESTING_OID(PE.OID), P.BLOCK_OID), 1, 15)  as NEST_ID,
            GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_PART_NEST,
            GET_NEST_HEIGHT (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_LENGTH,
            GET_NEST_WIDTH (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_WIDTH,
            GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_EQ_NEST,
            STD.WEB_HEIGHT as WH,
            STD.WEB_THICKNESS as WT,
            STD.FLANGE_HEIGHT as FH,
            STD.FLANGE_THICKNESS as FT,
            STD.STOCK_CODE0 as STOCK
        FROM
            PRD_EXPL_PART PE,
            PRD_PART P,
            INP_PROFILE_ATT_DB PRF,
            BLOCK B,
            STD_PROFILE STD
        WHERE
            (PRF.INP_PART_OID = P.OID AND PRF.STD_PROFILE_OID=STD.OID)
          AND PE.PRD_PART_OID= P.OID
          AND P.PART_TYPE IN (16,19,21,24)
          AND P.BLOCK_OID=B.OID
          AND P.BLOCK_OID in (SELECT OID FROM BLOCk WHERE DESCRIPTION like '%&drawingNumber %')
        union all
        select
            P.OID as PART_OID,
            PE.OID as PEOID,
            P.CODE as PART_CODE,
            DECODE (PE.SYMMETRY,  0,'P', 1,'S', 2,'C', 3,'') as SYMMETRY,
            P.PART_TYPE,
            B.CODE as BLOCK_CODE,
            RPT_COPY_COUNT (P.OID) as NUM_EQ_PART,
            GET_PART_DESC (P.OID, 1) as PART_DESC,
            'PL' as ELEM_TYPE,
            SUBSTR (GET_MATERIAL_CODE (PA.MATERIAL_OID, 1), 1, 12) as MATERIAL,
            RPT_GET_SD_PLATE_DIM (P.OID, 0) as LENGTH,
            RPT_GET_SD_PLATE_DIM (P.OID, 1) as WIDTH,
            PA.THICKNESS,
            RPT_GET_PART_WEIGHT (P.OID) as WEIGHT_UNIT,
            RPT_GET_PART_WEIGHT (P.OID) * RPT_COPY_COUNT (P.OID) as TOTAL_WEIGHT,
            SUBSTR (GET_NEST_ID (GET_PRD_EXPL_NESTING_OID(PE.OID), P.BLOCK_OID), 1, 15)  as NEST_ID,
            GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_PART_NEST,
            GET_NEST_HEIGHT (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_LENGTH,
            GET_NEST_WIDTH (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_WIDTH,
            GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_EQ_NEST,
            0 as WH,
            PA.THICKNESS as WT,
            0 as FH,
            0 as FT,
            (select STORAGE_CODE from STD_PLATE where MATERIAL_OID=PA.MATERIAL_OID and THICKNESS=PA.THICKNESS and STORAGE_CODE is not null  fetch first 1 row only  ) as STOCK
        FROM PRD_EXPL_PART PE,  PRD_PART P, V_RPT_SHDK_PLATE_ATT PA, BLOCK B
        WHERE
                PA.PRD_PART_OID = P.OID AND PE.PRD_PART_OID =PA.PRD_PART_OID
          AND P.PART_TYPE IN (9, 10)
          AND P.BLOCK_OID=B.OID
          and B.DESCRIPTION like '%&drawingNumber %'
        union all
        select  P.OID as PART_OID,   PE.OID as PEOID, P.CODE  as PART_CODE,
                DECODE (PE.SYMMETRY,  0,'P', 1,'S', 2,'C', 3,'') as SYMMETRY,
                P.PART_TYPE,
                B.CODE as BLOCK_CODE,
                RPT_COPY_COUNT (P.OID) as NUM_EQ_PART,
                GET_PART_DESC (P.OID, 1) as PART_DESC,
                SUBSTR (GET_SECT_TP (prf.std_profile_oid), 1, 2) as ELEM_TYPE,
                SUBSTR (GET_MATERIAL_CODE (PRF.STD_PROFILE_OID, 2), 1, 12) as MATERIAL,
                PRF.LENGTH * 1000 as LENGTH,
                STD.WEB_HEIGHT as WIDTH,
                STD.WEB_THICKNESS  as THICKNESS,
                RPT_GET_PART_WEIGHT (P.OID) as WEIGHT_UNIT,
                RPT_GET_PART_WEIGHT (P.OID) * RPT_COPY_COUNT (P.OID)  as TOTAL_WEIGHT,
                SUBSTR (GET_NEST_ID (GET_PRD_EXPL_NESTING_OID(PE.OID), P.BLOCK_OID), 1, 15)  as NEST_ID,
                GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_PART_NEST,
                GET_NEST_HEIGHT (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_LENGTH,
                GET_NEST_WIDTH (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_WIDTH,
                GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_EQ_NEST,
                STD.WEB_HEIGHT as WH,
                STD.WEB_THICKNESS as WT,
                STD.FLANGE_HEIGHT as FH,
                STD.FLANGE_THICKNESS as FT,
                STD.STOCK_CODE0 as STOCK
        FROM PRD_EXPL_PART PE, PRD_PART P, PRD_PROFILE PRF, BLOCK B,STD_PROFILE STD


        WHERE

            (PRF.PRD_PART_OID = P.OID AND PRF.STD_PROFILE_OID=STD.OID)
          AND P.OID=PE.PRD_PART_OID
          AND P.PART_TYPE IN (0,1,2,3,6)
          AND P.BLOCK_OID=B.OID
          AND B.DESCRIPTION like '%&drawingNumber %'
        union all
        select P.OID as PART_OID,   PE.OID as PEOID, P.CODE  as PART_CODE,
               DECODE (PE.SYMMETRY,  0,'P', 1,'S', 2,'C', 3,'') as SYMMETRY,
               P.PART_TYPE,
               B.CODE as BLOCK_CODE,
               RPT_COPY_COUNT (P.OID) as NUM_EQ_PART,
               SUBSTR (GET_PART_DESC (P.OID, 1), 1, 200) as PART_DESC,
               'PL' as ELEM_TYPE,
               SUBSTR (GET_MATERIAL_CODE (PA.MATERIAL_OID, 1), 1, 12) as MATERIAL,
               RPT_INP_GET_PLT_DIMENSIONS (P.OID, 0) * 1000 as LENGTH,
               RPT_INP_GET_PLT_DIMENSIONS (P.OID, 1) * 1000 as WIDTH,
               PA.THICKNESS * 1000   as THICKNESS,
               RPT_GET_PART_WEIGHT (P.OID) as WEIGHT_UNIT,
               RPT_GET_PART_WEIGHT (P.OID) * RPT_COPY_COUNT (P.OID) as TOTAL_WEIGHT,
               SUBSTR (GET_NEST_ID (GET_PRD_EXPL_NESTING_OID(PE.OID), P.BLOCK_OID), 1, 15)  as NEST_ID,
               GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_PART_NEST,
               GET_NEST_HEIGHT (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_LENGTH,
               GET_NEST_WIDTH (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_WIDTH,
               GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_EQ_NEST,
               0 as WH,
               PA.THICKNESS * 1000 as WT,
               0 as FH,
               0 as FT,
               (select STORAGE_CODE from STD_PLATE where MATERIAL_OID=PA.MATERIAL_OID and THICKNESS=PA.THICKNESS*1000 and STORAGE_CODE is not null fetch first 1 row only    ) as STOCK
        FROM PRD_EXPL_PART PE, PRD_PART P, INP_PLATE_ATT_DB PA,BLOCK B
        WHERE
                PA.INP_PART_OID = P.OID AND PE.PRD_PART_OID=P.OID
          AND P.PART_TYPE IN (14,
                              15,
                              18,
                              20,
                              22,
                              23,
                              25,
                              26)
          AND P.BLOCK_OID=B.OID
          and B.DESCRIPTION like '%&drawingNumber %'
        union all
        select
            PART.OID as PART_OID,
            PART.PEOID as PEOID,
            PART.CODE  as PART_CODE,
            DECODE (PART.SYMMETRY,  0,'P', 1,'S', 2,'C', 3,'') as SYMMETRY,
            PART.PART_TYPE,
            PART.BLOCK_CODE as BLOCK_CODE,
            RPT_COPY_COUNT (PART.OID) as NUM_EQ_PART,
            SUBSTR (GET_PART_DESC (PART.OID, 1), 1, 200) as PART_DESC,
            SUBSTR (GET_SECT_TP (PART.STD_SECTION_OID), 1, 2) as ELEM_TYPE,
            SUBSTR (GET_MATERIAL_CODE (PART.STD_SECTION_OID, 2), 1, 12) as MATERIAL,
            RPT_GET_PART_LENGTH (PART.OID) * 1000 as LENGTH,
            GET_PRF_W_HEIGHT (PART.STD_SECTION_OID) as WIDTH,
            GET_PRF_W_THICK (PART.STD_SECTION_OID)   as THICKNESS,
            RPT_GET_PART_WEIGHT (PART.OID) as WEIGHT_UNIT,
            RPT_GET_PART_WEIGHT (PART.OID) * RPT_COPY_COUNT (PART.OID) as TOTAL_WEIGHT,
            SUBSTR (GET_NEST_ID (GET_PRD_EXPL_NESTING_OID(PART.PEOID), PART.BLOKOID), 1, 15)  as NEST_ID,
            GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PART.PEOID)) as NUM_PART_NEST,
            GET_NEST_HEIGHT (GET_PRD_EXPL_NESTING_OID(PART.PEOID)) as NEST_LENGTH,
            GET_NEST_WIDTH (GET_PRD_EXPL_NESTING_OID(PART.PEOID)) as NEST_WIDTH,
            GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PART.PEOID)) as NUM_EQ_NEST,
            0 as WH,
            GET_PRF_W_THICK (PART.STD_SECTION_OID) as WT,
            0 as FH,
            0 as FT,
            (select STORAGE_CODE from STD_PLATE where MATERIAL_OID= PART.MAT_CODE and THICKNESS=GET_PRF_W_THICK (PART.STD_SECTION_OID) and STORAGE_CODE is not null fetch first 1 row only    ) as STOCK
        from
            (
                select
                    P.OID,
                    P.CODE,
                    P.PART_TYPE,
                    PE.OID as PEOID,
                    PE.SYMMETRY,
                    PRF.STD_SECTION_OID,
                    P.BLOCK_OID as BLOKOID,
                    (select CODE from BLOCK where OID=P.BLOCK_OID) as BLOCK_CODE,
                    (select MAT_CODE from PRD_NEST_GEN where OID = GET_PRD_EXPL_NESTING_OID(PE.OID)) as MAT_CODE,
                    GET_PRD_EXPL_NESTING_OID(PE.OID) as NOID
                from PRD_PART P, PRD_EXPL_PART PE, INP_LC_ATT_DB PRF
                where
                        P.PART_TYPE in (17)
                  AND P.BLOCK_OID=(select OID from BLOCK where DESCRIPTION like '%&drawingNumber %')
                  AND PE.PRD_PART_OID=  P.OID
                  AND PRF.INP_PART_OID = P.OID


            ) PART

        union all
        select P.OID as PART_OID,   PE.OID as PEOID, P.CODE  as PART_CODE,
               DECODE (PE.SYMMETRY,  0,'P', 1,'S', 2,'C', 3,'') as SYMMETRY,
               P.PART_TYPE,
               B.CODE as BLOCK_CODE,
               1 as NUM_EQ_PART,
               GET_PART_DESC (P.OID, 1) as PART_DESC,
               'PL' as ELEM_TYPE,
               PA.MATQ as MATERIAL,
               RPT_AS_GET_PLT_DIMENSIONS (P.OID, 0) * 1000 as LENGTH,
               RPT_AS_GET_PLT_DIMENSIONS (P.OID, 1) * 1000 as WIDTH,
               PA.THICKNESS,
               PE.WEIGHT as WEIGHT_UNIT,
               PE.WEIGHT as TOTAL_WEIGHT,
               SUBSTR (GET_NEST_ID (GET_PRD_EXPL_NESTING_OID(PE.OID), P.BLOCK_OID), 1, 15)  as NEST_ID,
               GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_PART_NEST,
               GET_NEST_HEIGHT (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_LENGTH,
               GET_NEST_WIDTH (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_WIDTH,
               GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_EQ_NEST,
               0 as WH,
               PA.THICKNESS as WT,
               0 as FH,
               0 as FT,
               (select STORAGE_CODE from STD_PLATE where MATERIAL_OID=M.OID and THICKNESS=PA.THICKNESS and STORAGE_CODE is not null fetch first 1 row only    ) as STOCK

        FROM PRD_PART               P,
             AS_STD_PART_PLATE      PA,
             PRD_EXPL_PART          PE ,  BLOCK B, MATERIAL M

        WHERE
                PA.OID = P.OID
          AND PE.PRD_PART_OID = P.OID
          AND P.PART_TYPE = 12 AND P.BLOCK_OID=B.OID
          AND PA.MATQ=M.CODE
          and B.DESCRIPTION like '%&drawingNumber %'
        union all
        select P.OID as PART_OID,   PE.OID as PEOID,  P.CODE  as PART_CODE,
               DECODE (PE.SYMMETRY,  0,'P', 1,'S', 2,'C', 3,'') as SYMMETRY,
               P.PART_TYPE,
               B.CODE as BLOCK_CODE,
               1 as NUM_EQ_PART,
               GET_PART_DESC (P.OID, 1) as PART_DESC,
               DECODE (STD.SECTION,
                       -1, '',
                       0, 'FS',
                       1, 'AS',
                       2, 'IS',
                       3, 'TS',
                       4, 'US',
                       5, 'BS',
                       6, 'ST',
                       7, 'AT',
                       8, 'OS',
                       9, 'PS',
                       10, 'RS',
                       11, 'MC',
                       12, 'DB',
                       13, 'SR',
                       14, 'HR',
                       15, 'LI',
                       16, 'ZL',
                       17, 'TL',
                       '') as ELEM_TYPE,
               SUBSTR (GET_MATERIAL_CODE (STD.MATERIAL_OID, 1), 1, 12) as MATERIAL,
               PRF.LENGTH as LENGTH,
               STD.WEB_HEIGHT as WIDTH,
               STD.WEB_THICKNESS,
               PE.WEIGHT as WEIGHT_UNIT,
               PE.WEIGHT as TOTAL_WEIGHT,
               SUBSTR (GET_NEST_ID (GET_PRD_EXPL_NESTING_OID(PE.OID), P.BLOCK_OID), 1, 15)  as NEST_ID,
               GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_PART_NEST,
               GET_NEST_HEIGHT (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_LENGTH,
               GET_NEST_WIDTH (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_WIDTH,
               GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_EQ_NEST,
               STD.WEB_HEIGHT as WH,
               STD.WEB_THICKNESS as WT,
               STD.FLANGE_HEIGHT as FH,
               STD.FLANGE_THICKNESS as FT,
               STD.STOCK_CODE0 as STOCK
        FROM PRD_PART               P,
             AS_STD_PART_PROF       PRF,
             STD_PROFILE            STD,
             PRD_EXPL_PART          PE,  BLOCK B

        WHERE  PRF.OID = P.OID
          AND PE.PRD_PART_OID = P.OID
          AND STD.KSE = PRF.KSE
          AND P.PART_TYPE = 7 AND P.BLOCK_OID=B.OID
          and B.DESCRIPTION like '%&drawingNumber %'
    union all
        SELECT
            P.OID as PART_OID,
            PE.OID as PEOID,
            P.CODE as PART_CODE,
            DECODE (PE.SYMMETRY,  0,'P', 1,'S', 2,'C', 3,'') as SYMMETRY,
            P.PART_TYPE,
            B.CODE as BLOCK_CODE,
            1 as NUM_EQ_PART,
            GET_PART_DESC (P.OID, 1) as PART_DESC,
            'PL' as ELEM_TYPE,
            PA.MATQ as MATERIAL,
            RPT_AS_GET_PLT_DIMENSIONS (P.OID, 0) * 1000 as LENGTH,
            RPT_AS_GET_PLT_DIMENSIONS (P.OID, 1) * 1000 as WIDTH,
            PA.THICKNESS,
            PE.WEIGHT as WEIGHT_UNIT,
            PE.WEIGHT as TOTAL_WEIGHT,
            SUBSTR (GET_NEST_ID (GET_PRD_EXPL_NESTING_OID(PE.OID), P.BLOCK_OID), 1, 15)  as NEST_ID,
            GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_PART_NEST,
            GET_NEST_HEIGHT (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_LENGTH,
            GET_NEST_WIDTH (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NEST_WIDTH,
            GET_NUM_EQ_NEST (GET_PRD_EXPL_NESTING_OID(PE.OID)) as NUM_EQ_NEST,
            0 as WH,
            PA.THICKNESS as WT,
            0 as FH,
            0 as FT,
            (select STORAGE_CODE from STD_PLATE where MATERIAL_OID=M.OID and THICKNESS=PA.THICKNESS and STORAGE_CODE is not null fetch first 1 row only) as STOCK
        FROM
            PRD_PART               P,
            AS_PART_CONT_PLATE     PA,
            V_RPT_NEST_PART_GROUP  NP,
            PRD_EXPL_PART          PE,
            BLOCK B,
            MATERIAL M
        WHERE
                PA.OID = P.OID
          AND NP.PART_OID = P.OID
          AND PE.PRD_PART_OID = P.OID
          AND P.BLOCK_OID = B.OID
          AND P.PART_TYPE = 12
          AND PA.MATQ = M.CODE
          and B.DESCRIPTION like '%&drawingNumber %'
    ) P
order by P.PART_CODE,P.SYMMETRY
  
