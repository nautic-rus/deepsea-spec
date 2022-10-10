SELECT
    MOD_OID,
    AS_OID,
    WEIGHT,
    SURFACE,
    USERID,
    CODEID,
    (SELECT MATQ FROM AS_SPE_GEO_BASIC WHERE OID = MOD_OID) AS MATERIAL,
    (SELECT DESCR FROM MATERIAL_QUALITY WHERE CODE IN (SELECT MATQ FROM AS_SPE_GEO_BASIC WHERE OID = MOD_OID)) AS MATERIAL_DESCRIPTION,
    (SELECT OBJ_TYPE FROM AS_GEOM_BASIC WHERE PARENT_OID = MOD_OID AND GROUP_OID = AS_OID) AS OBJ_TYPE,
    (SELECT PAR1 FROM AS_GEOM_BASIC WHERE PARENT_OID = MOD_OID AND GROUP_OID = AS_OID) AS PAR1,
    (SELECT PAR2 FROM AS_GEOM_BASIC WHERE PARENT_OID = MOD_OID AND GROUP_OID = AS_OID) AS PAR2,
    (SELECT PAR3 FROM AS_GEOM_BASIC WHERE PARENT_OID = MOD_OID AND GROUP_OID = AS_OID) AS PAR3,
    (SELECT PAR4 FROM AS_GEOM_BASIC WHERE PARENT_OID = MOD_OID AND GROUP_OID = AS_OID) AS PAR4,
    (SELECT PAR5 FROM AS_GEOM_BASIC WHERE PARENT_OID = MOD_OID AND GROUP_OID = AS_OID) AS PAR5,
    (SELECT PAR6 FROM AS_GEOM_BASIC WHERE PARENT_OID = MOD_OID AND GROUP_OID = AS_OID) AS PAR6,
    (SELECT PAR7 FROM AS_GEOM_BASIC WHERE PARENT_OID = MOD_OID AND GROUP_OID = AS_OID) AS PAR7,
    (SELECT PAR8 FROM AS_GEOM_BASIC WHERE PARENT_OID = MOD_OID AND GROUP_OID = AS_OID) AS PAR8,
    (SELECT WEIGHT FROM BS_DESIGN_ATOM WHERE BS_DESIGN_NODE_OID IN (SELECT OID FROM BS_DESIGN_NODE WHERE MODEL_OID = MOD_OID)) AS BS_WEIGHT,
    (SELECT NAME FROM ZONE WHERE OID IN (SELECT ZONE_OID FROM BS_DESIGN_ATOM WHERE BS_DESIGN_NODE_OID IN (SELECT OID FROM BS_DESIGN_NODE WHERE MODEL_OID = MOD_OID))) AS ZONE,
    (SELECT X_MIN FROM BS_DESIGN_ATOM WHERE BS_DESIGN_NODE_OID IN (SELECT OID FROM BS_DESIGN_NODE WHERE MODEL_OID = MOD_OID)) AS X_MIN,
    (SELECT Y_MIN FROM BS_DESIGN_ATOM WHERE BS_DESIGN_NODE_OID IN (SELECT OID FROM BS_DESIGN_NODE WHERE MODEL_OID = MOD_OID)) AS Y_MIN,
    (SELECT Z_MIN FROM BS_DESIGN_ATOM WHERE BS_DESIGN_NODE_OID IN (SELECT OID FROM BS_DESIGN_NODE WHERE MODEL_OID = MOD_OID)) AS Z_MIN,
    (SELECT X_MAX FROM BS_DESIGN_ATOM WHERE BS_DESIGN_NODE_OID IN (SELECT OID FROM BS_DESIGN_NODE WHERE MODEL_OID = MOD_OID)) AS X_MAX,
    (SELECT Y_MAX FROM BS_DESIGN_ATOM WHERE BS_DESIGN_NODE_OID IN (SELECT OID FROM BS_DESIGN_NODE WHERE MODEL_OID = MOD_OID)) AS Y_MAX,
    (SELECT Z_MAX FROM BS_DESIGN_ATOM WHERE BS_DESIGN_NODE_OID IN (SELECT OID FROM BS_DESIGN_NODE WHERE MODEL_OID = MOD_OID)) AS Z_MAX,
    PROFILEKSE,
    (SELECT STOCK_CODE0 FROM STD_PROFILE WHERE KSE = PROFILEKSE AND ROWNUM = 1) AS PROFILE_STOCK,
    (SELECT STORAGE_CODE FROM STD_PLATE WHERE MATERIAL_OID IN (SELECT OID FROM MATERIAL WHERE THICKNESS = PLATETHICKNESS AND CODE = PLATEMAT)) AS PLATE_STOCK,
    (SELECT CODE FROM MATERIAL WHERE OID IN (SELECT MATERIAL_OID FROM STD_PROFILE WHERE KSE = PROFILEKSE)) AS PROFILE_MATERIAL,
    PLATEMAT AS PLATE_MATERIAL
FROM(
        SELECT
            MOD_OID,
            AS_OID,
            WEIGHT,
            SURFACE,
            USERID,
            CODEID,
            (SELECT KSE FROM AS_STD_PART_PROF WHERE OID = MOD_OID) AS PROFILEKSE,
            (SELECT MATQ FROM AS_STD_PART_PLATE WHERE OID = MOD_OID) AS PLATEMAT,
            (SELECT THICKNESS FROM AS_STD_PART_PLATE WHERE OID = MOD_OID) AS PLATETHICKNESS
        FROM(
                SELECT
                    OID AS MOD_OID,
                    AS_OID,
                    WEIGHT,
                    SURFACE,
                    USERID,
                    CODEID
                FROM
                    AS_ELEM
                WHERE
                        AS_OID IN (SELECT OID FROM AS_LIST WHERE USERID = '&docNumberSuffix')
            )
    )
