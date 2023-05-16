select
    pcomp.*,
    (select descr from OBJ_CLASS_LANG where ELEM_CLASS in (select OID from OBJ_CLASS where name = pcomp.CLASSNAME) and ELEM_CLASS in (select ELEM_CLASS from COMPONENT where STOCK_CODE = pcomp.STOCKCODE)) AS CLASSDESCR
from
    v_pipecomp pcomp
