select
    el.comp, el.userid, (select name from zone where oid = el.zone) as zone, cmp.stock_code
from element el, component cmp
where
    el.system in (select oid from systems where name = '&system') and el.type = 10 and
    el.COMP = cmp.OID and
    cmp.oid in (select comp from component_elec where mechanical = 0) and cmp.stock_code is not null
