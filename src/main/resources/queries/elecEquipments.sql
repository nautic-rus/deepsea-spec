select
  e.oid,
  e.userid,
  mdl.label,
  mdl.x,
  mdl.y,
  mdl.z,
  mdl.vx,
  mdl.vy,
  mdl.vz,
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
  eap.a43
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
  left join model m on m.oid_f_lib_model = cp.model
  left join mdlconnector mdl on mdl.oid_model = m.oid
  and mdl.label like ('CP')
  left join elem_pos ep on ep.elem = e.oid
  left join elem_abs_pos eap on eap.oid = ep.oid
