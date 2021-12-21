SELECT
    PRD.OID AS PARTOID,
    PRD.CODE AS PARTNAME,
    PRD.DESCRIPTION AS DESCRIPTION,
    BLOCK.CODE AS BLOCKNAME,
    EXPL.WEIGHT AS WEIGHT,
    EXPL.X_COG AS X_COG,
    EXPL.Y_COG AS Y_COG,
    EXPL.Z_COG AS Z_COG,
    0 AS KSEOID
FROM
    BLOCK BLOCK,
    PRD_PART PRD,
    PRD_EXPL_PART EXPL,
    AS_STD_PART_PLATE ASPLATE
WHERE
    BLOCK.OID = PRD.BLOCK_OID AND
    EXPL.PRD_PART_OID = PRD.OID AND
    ASPLATE.OID = PRD.OID
UNION ALL
SELECT
    PRD.OID AS PARTOID,
    PRD.CODE AS PARTNAME,
    PRD.DESCRIPTION AS DESCRIPTION,
    BLOCK.CODE AS BLOCKNAME,
    EXPL.WEIGHT AS WEIGHT,
    EXPL.X_COG AS X_COG,
    EXPL.Y_COG AS Y_COG,
    EXPL.Z_COG AS Z_COG,
    0 AS KSEOID
FROM
    BLOCK BLOCK,
    PRD_PART PRD,
    PRD_EXPL_PART EXPL,
    AS_PART_CONT_PLATE ASCONTPLATE
WHERE
    BLOCK.OID = PRD.BLOCK_OID AND
    EXPL.PRD_PART_OID = PRD.OID AND
    ASCONTPLATE.OID = PRD.OID
UNION ALL
SELECT
    PRD.OID AS PARTOID,
    PRD.CODE AS PARTNAME,
    PRD.DESCRIPTION AS DESCRIPTION,
    BLOCK.CODE AS BLOCKNAME,
    EXPL.WEIGHT AS WEIGHT,
    EXPL.X_COG AS X_COG,
    EXPL.Y_COG AS Y_COG,
    EXPL.Z_COG AS Z_COG,
    STDPROF.OID AS KSEOID
FROM
    BLOCK BLOCK,
    PRD_PART PRD,
    PRD_EXPL_PART EXPL,
    AS_STD_PART_PROF ASPROF,
    STD_PROFILE STDPROF
WHERE
    BLOCK.OID = PRD.BLOCK_OID AND
    EXPL.PRD_PART_OID = PRD.OID AND
    ASPROF.OID = PRD.OID AND
    STDPROF.KSE = ASPROF.KSE
UNION ALL
SELECT
    PRD.OID AS PARTOID,
    PRD.CODE AS PARTNAME,
    PRD.DESCRIPTION AS PARTDESCR,
    BLOCK.CODE AS BLOCKNAME,
    EXPL.WEIGHT AS WEIGHT,
    EXPL.X_COG AS X_COG,
    EXPL.Y_COG AS Y_COG,
    EXPL.Z_COG AS Z_COG,
    SP.KSE_OID AS KSEOID
FROM
    BLOCK BLOCK,
    PRD_PART PRD,
    PRD_EXPL_PART EXPL,
    INP_SINGLE_PART_DB SP
WHERE
    BLOCK.OID = PRD.BLOCK_OID AND
    EXPL.PRD_PART_OID = PRD.OID AND
    SP.PRD_PART_OID = PRD.OID AND
    SP.KSE_OID IS NOT NULL
UNION ALL
SELECT
    PRD.OID AS PARTOID,
    PRD.CODE AS PARTNAME,
    PRD.DESCRIPTION AS PARTDESCR,
    BLOCK.CODE AS BLOCKNAME,
    EXPL.WEIGHT AS WEIGHT,
    EXPL.X_COG AS X_COG,
    EXPL.Y_COG AS Y_COG,
    EXPL.Z_COG AS Z_COG,
    0 AS KSEOID
FROM
    BLOCK BLOCK,
    PRD_PART PRD,
    PRD_EXPL_PART EXPL,
    INP_SINGLE_PART_DB SP
WHERE
    BLOCK.OID = PRD.BLOCK_OID AND
    EXPL.PRD_PART_OID = PRD.OID AND
    SP.PRD_PART_OID = PRD.OID AND
    SP.KSE_OID IS NULL