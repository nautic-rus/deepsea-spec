select
    elm.userid,
    pls.tray_fitting,
    pls.type as type,
    pls.zone as zone,
    pls.system as system,
    pls.line as line,
    pls.pls as pls,
    pls.elem as elem,
    pls.ctype as ctype,
    pls.idsq as idsq,
    pls.node1 as node1,
    pls.node2 as node2,
    pls.x_cog as x_cog,
    pls.y_cog as y_cog,
    pls.z_cog as z_cog,
    zn.name as zone_name,
    (select vcs.stock_code from v_cabletray_stockcode vcs where vcs.system = pls.system and vcs.zone = pls.zone and vcs.line = pls.line and vcs.pls = pls.pls and vcs.stock_code is not null and rownum = 1) as stock_code,
    tr.comp_stock
from pls_elem pls
         left join element elm on elm.oid = pls.oid
         left join v_transit tr on tr.oid = pls.tray_fitting and tr.zone = pls.ZONE and tr.syst = pls.system
         left join zone zn on elm.zone = zn.oid
where pls.system in (select seqid from systems where name in (&systemList))
