SELECT
    N.OID,
    N.TYPE,
    N.NAME,
    N.DESCRIPTION,
    N.PARENT_NODE,
    N.ATOM_TYPE,
    N.BLOCK_OID,
    N.WEIGHT,
    N.X_COG,
    N.Y_COG,
    N.Z_COG,
    (SELECT NAME FROM BS_NODE_TYPE WHERE ATOM_TYPE = N.ATOM_TYPE) AS ATOM_NAME,
    (SELECT DNA FROM V_BS_DESIGN_ATOM_ADN WHERE ATOM_OID = N.BS_DESIGN_ATOM_OID) AS DNA
FROM
    V_BS_DESIGN_NODE N
WHERE
    N.ATOM_TYPE IS NOT NULL AND
    NOT(N.ATOM_TYPE = 1 AND NAME LIKE '#%')