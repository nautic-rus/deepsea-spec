select
    sys.name as system,
    (select USERID from ELEMENT where UUID=PE.UUID) as USERID,
	pe.idsq as oid,
	z.name as zone,
    pe.line,
    pe.weight,
    pe.X_COG,
    pe.Y_COG,
    pe.Z_COG,
    vcpl.seal_type,
    vcpl.type,
    vcpl.code,
    vcpl.stock_code,
    vcpl.descr,
    n1.userid as node_1,
    n1.x * 1000 as n1_x,
    n1.y * 1000 as n1_y,
    n1.z * 1000 as n1_z,
    n2.userid as node_2,
    n2.x * 1000 as n2_x,
    n2.y * 1000 as n2_y,
    n2.z * 1000 as n2_z,
    sqrt(power((n2.X - n1.X), 2) + power((n2.Y - n1.Y), 2) + power((n2.Z - n1.Z), 2)) * 1000 as length
from
    systems sys,
	pls_elem PE,
	zone z,
    v_cable_penetration_library vcpl,
    cn002.node n1,
    cn002.node n2,
    cn002.segment s
where
    sys.seqid = pe.system and
    z.seqid = pe.zone and
	pe.tray_fitting = vcpl.oid and
	((s.node1 = pe.node2 and
    s.node2 = pe.node1) or
    (s.node1 = pe.node1 and
    s.node2 = pe.node2)) and
    pe.node1 = n1.seqid and
    pe.node2 = n2.seqid
	and sys.name in :docNumber