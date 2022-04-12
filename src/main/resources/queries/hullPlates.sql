SELECT *
FROM
    (
        SELECT
            CODE,
            BLOCK,
            CONCAT('PL', CAST(THICKNESS AS VARCHAR(10))) AS NAME,
            DESCRIPTION,
            WEIGHT,
            THICKNESS,
            MAT
        FROM
            (
                SELECT
                    P.CODE AS CODE,
                    (SELECT CODE FROM BLOCK WHERE OID = P.BLOCK_OID) AS BLOCK,
                    CASE WHEN P.DESCRIPTION IS NULL THEN 'Aux Plate' ELSE P.DESCRIPTION END AS DESCRIPTION,
                    PEP.WEIGHT,
                    (
                        SELECT THICKNESS AS THK FROM PRD_PLATE_ATT WHERE PRD_PART_OID = P.OID
                        UNION ALL
                        SELECT THICKNESS AS THK FROM SHDK_STD_PLATE WHERE PRD_PART_OID = P.OID
                        UNION ALL
                        SELECT THICKNESS * 1000 AS THK FROM INP_PLATE_ATT_DB WHERE INP_PART_OID = P.OID
                        UNION ALL
                        SELECT THICKNESS AS THK FROM AS_STD_PART_PLATE WHERE OID = P.OID
                    ) AS THICKNESS,
                    (
                        SELECT CODE AS MAT FROM MATERIAL WHERE OID = (SELECT MATERIAL_OID FROM PRD_PLATE_ATT WHERE PRD_PART_OID = P.OID)
                        UNION ALL
                        SELECT CODE AS MAT FROM MATERIAL WHERE OID = (SELECT MATERIAL_OID FROM SHDK_STD_PLATE WHERE PRD_PART_OID = P.OID)
                        UNION ALL
                        SELECT CODE AS MAT FROM MATERIAL WHERE OID = (SELECT MATERIAL_OID FROM INP_PLATE_ATT_DB WHERE INP_PART_OID = P.OID)
                        UNION ALL
                        SELECT MATQ AS MAT FROM AS_STD_PART_PLATE
                        WHERE OID = P.OID
                    ) AS MAT
                FROM
                    PRD_PART P, PRD_EXPL_PART PEP
                WHERE
                        P.PART_TYPE IN (8, 9, 10, 11, 12, 13, 14, 15, 18, 20, 22, 23, 25, 26, 31, 16)
                  AND P.BLOCK_OID NOT IN (SELECT OID FROM BLOCK WHERE CODE LIKE 'L%')
                  AND PEP.PRD_PART_OID = P.OID
            )
        UNION ALL
        SELECT
            P.CODE,
            P.BLOCK,
            CONCAT(
                    DECODE
                        (
                            STD.SECTION,
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
                            ''
                        ),
                    CONCAT(CONCAT(CONCAT(CONCAT(CAST(STD.WEB_HEIGHT AS VARCHAR(10)), 'x'), CONCAT(CAST(STD.WEB_THICKNESS AS VARCHAR(10)), 'x')), CONCAT(CAST(STD.FLANGE_HEIGHT AS VARCHAR(10)), 'x')), CAST(STD.FLANGE_THICKNESS AS VARCHAR(10)))
                ) AS NAME,
            P.DESCRIPTION,
            P.WEIGHT,
            (SELECT WEB_THICKNESS FROM STD_PROFILE WHERE KSE = GKSE) AS THICKNESS,
            (SELECT CODE FROM MATERIAL WHERE OID =(SELECT MATERIAL_OID FROM STD_PROFILE WHERE KSE = GKSE)) AS MATERIAL
        FROM
            (SELECT
                 P.CODE AS CODE,
                 (SELECT CODE FROM BLOCK WHERE OID = P.BLOCK_OID) AS BLOCK,
                 P.DESCRIPTION AS DESCRIPTION,
                 PEP.WEIGHT AS WEIGHT,
                 (
                     SELECT STD.kse AS KSE FROM STD_PROFILE STD, PRD_PROFILE PRF WHERE STD.OID=PRF.STD_PROFILE_OID AND PRF.PRD_PART_OID = P.OID AND STD.SECTION = 0
                     UNION ALL
                     SELECT STD.kse AS KSE FROM STD_PROFILE STD, INP_PROFILE_ATT_DB PRF WHERE STD.OID=PRF.STD_PROFILE_OID AND PRF.INP_PART_OID = P.OID AND STD.SECTION = 0
                     UNION ALL
                     SELECT STD.kse AS KSE FROM STD_PROFILE STD, INP_LC_ATT_DB PRF WHERE STD.OID=PRF.STD_SECTION_OID AND PRF.INP_PART_OID = P.OID AND STD.SECTION = 0
                     UNION ALL
                     SELECT STD.kse AS KSE FROM STD_PROFILE STD, AS_STD_PART_PROF PRF WHERE STD.KSE=PRF.KSE AND PRF.OID=P.OID  AND STD.SECTION = 0
                 ) AS GKSE
             FROM
                 PRD_PART P, PRD_EXPL_PART PEP
             WHERE
                     P.PART_TYPE IN (17, 19, 24, 21) AND
                     P.BLOCK_OID NOT IN (SELECT OID FROM BLOCK WHERE CODE LIKE 'L%') AND
                     PEP.PRD_PART_OID = P.OID) P,
            STD_PROFILE STD
        WHERE
                GKSE IS NOT NULL AND
                STD.KSE = P.GKSE
    )
WHERE THICKNESS = &thickness AND MAT = '&material'