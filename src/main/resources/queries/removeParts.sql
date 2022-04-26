DELETE
FROM BS_NODE
WHERE OID IN
    (SELECT OID FROM V_BS_NODE WHERE BS_DESIGN_NODE_OID IN
            (SELECT OID FROM BS_DESIGN_NODE WHERE MODEL_OID IN
                      (SELECT OID FROM PRD_EXPL_PART WHERE PRD_PART_OID IN (SELECT
                                                                                PRD.OID
                                                                            FROM
                                                                                BLOCK BLOCK,
                                                                                PRD_PART PRD,
                                                                                INP_SINGLE_PART_DB SP
                                                                            WHERE
                                                                                    BLOCK.CODE LIKE '&block' AND
                                                                                    BLOCK.OID = PRD.BLOCK_OID AND
                                                                                    SP.PRD_PART_OID = PRD.OID AND
                                                                                    PRD.CODE IN (&parts)))));

DELETE
FROM BS_DESIGN_NODE
WHERE MODEL_OID IN (SELECT OID FROM PRD_EXPL_PART WHERE PRD_PART_OID IN (SELECT
                                                                             PRD.OID
                                                                         FROM
                                                                             BLOCK BLOCK,
                                                                             PRD_PART PRD,
                                                                             INP_SINGLE_PART_DB SP
                                                                         WHERE
                                                                                 BLOCK.CODE LIKE '&block' AND
                                                                                 BLOCK.OID = PRD.BLOCK_OID AND
                                                                                 SP.PRD_PART_OID = PRD.OID AND
                                                                                 PRD.CODE IN (&parts)));

DELETE
FROM PRD_PART
WHERE OID  IN (SELECT
                   PRD.OID
               FROM
                   BLOCK BLOCK,
                   PRD_PART PRD,
                   INP_SINGLE_PART_DB SP
               WHERE
                       BLOCK.CODE LIKE '&block' AND
                       BLOCK.OID = PRD.BLOCK_OID AND
                       SP.PRD_PART_OID = PRD.OID AND
                       PRD.CODE IN (&parts));