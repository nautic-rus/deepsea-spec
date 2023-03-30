select
  sl.descr as system,
  c.code,
  cs.descr as description,
  SUBSTR (
    get_cable_composition (ss.spec, ss.nom_sect),
    1,
    20
  ) as nom_section,
  ss.o_diameter as diameter,
  seg.code as seg_code,
  c.f_rout as f_rout,
   get_cab_length (
    c.seqid, c.from_e, c.to_e, c.f_rout,
    c.perc_corr, c.ext_len_1, c.ext_len_2,
    c.est_leng
  ) as length,
  c.perc_corr as l_correction,
  sl1.descr as from_system,
  n1.userid as from_eq_id,
  n1.x * 1000 as from_x,
  n1.y * 1000 as from_y,
  n1.z * 1000 as from_z,
  el1.descr as from_eq_desc,
  ele1.userid as from_eq,
  z1.userid as from_zone,
  zl1.descr from_zone_desc,
  sl2.descr as to_system,
  n2.userid as to_eq_id,
  n2.x * 1000 as to_x,
  n2.y * 1000 as to_y,
  n2.z * 1000 as to_z,
  el2.descr as to_eq_desc,
  ele2.userid as to_eq,
  z2.userid as to_zone,
  zl2.descr to_zone_desc,
  get_cab_route_area (c.seqid) as route_area,
  ss.codenumber as stock_code
from
  (select * from cable where syst in (select system from systems_lang where descr like (:docNumber))) c
  left join section_spec ss on ss.spec = c.spec
  and ss.nom_sect = c.sect
  left join cab_spec cs on cs.seqid = ss.spec
  and cs.seqid = c.spec
  left join systems_lang sl on sl.system = c.syst and sl.lang = '-2'
  left join segregation seg on seg.seqid = c.segr
  left join cab_route cr1 on cr1.cable = c.seqid
  and cr1.seq_pos = (
    select
      min(seq_pos)
    from
      cab_route
    where
      cable = c.seqid
  )
  left join node n1 on n1.seqid = cr1.node1
  left join rout_area ra1 on ra1.seqid = n1.r_area
  left join cab_route cr2 on cr2.cable = c.seqid
  and cr2.seq_pos = (
    select
      max(seq_pos)
    from
      cab_route
    where
      cable = c.seqid
  )
  left join node n2 on n2.seqid = cr2.node2
  left join rout_area ra2 on ra2.seqid = n2.r_area
  left join element ele1 on ele1.oid = c.from_e
  left join systems_lang sl1 on sl1.system = ele1.system and sl1.lang = '-2'
  left join element_lang el1 on el1.elem = ele1.oid and el1.lang = '-2'
  left join element_elec ee1 on ee1.elem = c.from_e
  left join zone z1 on z1.oid = ele1.zone
  left join zone_lang zl1 on zl1.zone = z1.oid
  left join element ele2 on ele2.oid = c.to_e
  left join systems_lang sl2 on sl2.system = ele2.system and sl2.lang = '-2'
  left join element_lang el2 on el2.elem = ele2.oid and el2.lang = '-2'
  left join element_elec ee2 on ee2.elem = c.to_e
  left join zone z2 on z2.oid = ele2.zone
  left join zone_lang zl2 on zl2.zone = z2.oid
  