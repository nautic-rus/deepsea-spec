select
    sys.name as system,
    SUM(pe.weight) as weight,
    vcpl.stock_code as stock_code,
    vcpl.descr as tray_desc,
    SUM(
        SQRT(
            POWER((n2.X - n1.X), 2) +
            POWER((n2.Y - n1.Y), 2) +
            POWER((n2.Z - n1.Z), 2)
            ) * 1000
        ) as LENGTH
from
    cn002.systems sys,
    cn002.pipeline_segment ps,
    cn002.zone z,
    cn002.pls_elem pe,
    cn002.segment s,
    cn002.v_ctray_pattern_level vcpl,
    cn002.node n1,
    cn002.node n2
where
    sys.seqid = pe.system and
    .seqid = pe.zone and
    pe.type = ps.type and
    pe.zone = ps.zone and
    pe.system = ps.system and
    pe.line = ps.line and
    pe.pls = ps.sqid and
    ((s.node1 = pe.node2 and
    s.node2 = pe.node1) or
    (s.node1 = pe.node1 and
    s.node2 = pe.node2)) and
    s.pattern = vcpl.seqid and
    pe.node1 = n1.seqid and
    pe.node2 = n2.seqid
    and sys.name in :docNumber
group by
    sys.name,
    vcpl.stock_code,
    vcpl.descr
