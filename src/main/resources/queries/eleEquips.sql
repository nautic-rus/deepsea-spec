select * from element elm
left join component cmp on cmp.oid = elm.comp
where elm.type = 10 and elm.zone in (select oid from zone where name in (&zoneList)) and elm.comp in (select comp from component_elec where mechanical = 0)
