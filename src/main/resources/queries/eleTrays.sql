select * from pls_elem pls
left join v_cabletray_stockcode vcs on vcs.system = pls.system and vcs.zone = pls.zone and vcs.line = pls.line and vcs.pls = pls.pls and vcs.elem = pls.elem
left join element elm on elm.oid = pls.oid
left join v_transit tr on tr.oid = pls.tray_fitting and tr.zone = pls.ZONE and tr.syst = pls.system
left join zone zn on elm.zone = zn.oid
where pls.system in (select seqid from systems where name in (&systemList))
