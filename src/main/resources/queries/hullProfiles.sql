SELECT *
FROM
    (
        SELECT
            PARTS.STD_KSE AS KSE,
            DECODE (PARTS.STD_SECTION,
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
                    18, 'AI',
                    19, 'BL',
                    20, 'LA',
                    21, 'TA',
                    '') AS SECTION,
            PARTS.MAT_CODE AS MATERIAL,
            PARTS.STD_WEB_HEIGHT AS W_H,
            PARTS.STD_WEB_THICKNESS AS W_T,
            PARTS.STD_FLANGE_HEIGHT AS F_H,
            PARTS.STD_FLANGE_THICKNESS AS F_T,
            PARTS.BL_CODE AS BLOCK,
            PARTS.PRF_LENGTH AS LENGTH,
            STDPROF.AREA AS AREA,
            STDPROF.STOCK_CODE0 AS STOCK,
            MAT.DENSITY,
            (PARTS.PRF_LENGTH * STDPROF.AREA * MAT.DENSITY / 100) AS WEIGHT
        FROM
            V_RPT_ALL_PROF_DATA PARTS,
            STD_PROFILE STDPROF,
            MATERIAL MAT
        WHERE
                BL_CODE NOT LIKE 'L%' AND
                PARTS.STD_KSE = STDPROF.KSE AND
                MAT.OID = STDPROF.MATERIAL_OID
    )
WHERE KSE = &kse AND MATERIAL = &material