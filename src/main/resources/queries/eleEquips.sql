select * from element elm
                  left join component cmp on cmp.oid = elm.comp
                  left join zone zn on elm.zone = zn.oid
where
    elm.type = 10 and
    elm.zone in (select oid from zone where name in (&zoneList)) and
    elm.comp in (select comp from component_elec where mechanical = 0) and
    elm.oid in (select elem from elem_pos)
