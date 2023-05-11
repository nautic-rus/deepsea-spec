select
    e.oid,
    e.type,
    e.userid,
    z.seqid as zone_seqid,
    z.name as zone_name,
    zl.descr as zone_descr,
    s.seqid as system_seqid,
    s.name as system_name,
    sl.descr as system_descr,
    cp.abbrev,
    cp.weight,
    cp.stock_code,
    oc.name as class_name,
    ra.code as ra_code,
    ra.descr as ra_descr,
    n.userid as node_userid,
    el.long_descr as eqelec,
    elempos.xcog(e.oid, e.type, e.deck, e.comp) as xcog,
    elempos.ycog(e.oid, e.type, e.deck, e.comp) as ycog,
    elempos.zcog(e.oid, e.type, e.deck, e.comp) as zcog,
    eap.a11,
    eap.a12,
    eap.a13,
    eap.a21,
    eap.a22,
    eap.a23,
    eap.a31,
    eap.a32,
    eap.a33,
    eap.a41,
    eap.a42,
    eap.a43,
    mdl.x * 1000 as x,
    mdl.y * 1000 as y,
    mdl.z * 1000 as z,
    (
      select
        bn2.name
      from
        bs_design_node bdn,
        bs_design_atom bda,
        bs_atom_fixed_attribute baf,
        bs_node bn,
        bs_node bn2
      where
        bdn.model_oid = e.oid
        and bda.bs_design_node_oid = bdn.oid
        and baf.bs_ds_atom_oid = bda.oid
        and bn.oid = baf.bs_node_oid
        and bn2.oid = bn.parent_node
    ) as surface
  from
    (
      select
        *
      from
        element
      where
        system in (
          select
            system
          from
            systems_lang
          where descr like (:docNumber)
        )
    ) e
    left join component cp on cp.oid = e.comp
    left join obj_class oc on oc.oid = cp.elem_class
    left join model m on m.oid_f_lib_model = cp.model
    left join mdlconnector mdl on mdl.oid_model = m.oid
    and mdl.label like ('CP')
    left join elem_pos ep on ep.elem = e.oid
    left join elem_abs_pos eap on eap.oid = ep.oid
    left join element_lang el on el.elem = e.oid
    and el.lang = -1
    left join zone z on z.oid = e.zone
    left join zone_lang zl on zl.zone = e.zone
    and zl.lang = -2
    left join node_elem ne on ne.elem = e.oid
    left join node n on n.seqid = ne.node
    left join rout_area ra on ra.seqid = n.r_area
    left join systems s on s.oid = e.system
    left join systems_lang sl on sl.system = e.system
    and sl.lang = -2

