select
  s.name as system,
  c.code,
  cs.descr,
  SUBSTR (
    get_cable_composition (ss.spec, ss.nom_sect),
    1,
    20
  ) as nom_section,
  ss.o_diameter,
  seg.code as seg_code,
  c.f_rout as f_rout,
   get_cab_length (
    c.seqid, c.from_e, c.to_e, c.f_rout,
    c.perc_corr, c.ext_len_1, c.ext_len_2,
    c.est_leng
  ) as length,
  n1.userid as from_node_userid,
  el1.descr as from_node_desc,
  ra1.code as from_node_room,
  ele1.userid as from_elem_userid,
  z1.userid as from_elem_room,
  zl1.descr from_elem_room_desc,
  n2.userid as to_node_userid,
  el2.descr as to_node_desc,
  ra2.code as to_node_room,
  ele2.userid as to_elem_userid,
  z2.userid as to_elem_room,
  zl2.descr to_elem_room_desc,
  get_cab_route_area (c.seqid) as cab_route_area,
  ss.codenumber as stockcode
from
  (select * from cable where syst in (select oid from systems where name in (:docNumber))) c
  join section_spec ss on ss.spec = c.spec
  and ss.nom_sect = c.sect
  join cab_spec cs on cs.seqid = ss.spec
  and cs.seqid = c.spec
  join systems_lang sl on sl.system = c.syst
  join segregation seg on seg.seqid = c.segr
  join cab_route cr1 on cr1.cable = c.seqid
  and cr1.seq_pos = (
    select
      min(seq_pos)
    from
      cab_route
    where
      cable = c.seqid
  )
  join node n1 on n1.seqid = cr1.node1
  join rout_area ra1 on ra1.seqid = n1.r_area
  join cab_route cr2 on cr2.cable = c.seqid
  and cr2.seq_pos = (
    select
      max(seq_pos)
    from
      cab_route
    where
      cable = c.seqid
  )
  join node n2 on n2.seqid = cr2.node2
  join rout_area ra2 on ra2.seqid = n2.r_area
  join element ele1 on ele1.oid = c.from_e
  join element_lang el1 on el1.elem = ele1.oid and el1.lang = '-2'
  join element_elec ee1 on ee1.elem = c.from_e
  join zone z1 on z1.oid = ele1.zone
  join zone_lang zl1 on zl1.zone = z1.oid
  join element ele2 on ele2.oid = c.to_e
  join element_lang el2 on el2.elem = ele2.oid and el2.lang = '-2'
  join element_elec ee2 on ee2.elem = c.to_e
  join zone z2 on z2.oid = ele2.zone
  join zone_lang zl2 on zl2.zone = z2.oid
  join systems s on s.oid = c.syst and s.oid = ele1.system
  