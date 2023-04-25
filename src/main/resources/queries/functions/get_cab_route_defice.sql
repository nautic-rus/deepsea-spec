CREATE OR REPLACE function CP701.get_cab_route_defice (cab_seqid number) return varchar2 as
 N PLS_INTEGER;
 InstallNode PLS_INTEGER;
 Ruta VARCHAR2(4000);
 Nodo VARCHAR2(17);
 CURSOR C1 (cab_seqid NUMBER) IS
 select node, nodid from v_cab_route2 where
	seqid = cab_seqid and type != '5' and (type != '2' or ncon != 2) order by rout_pos;
begin
 Ruta := '';
 InstallNode := 0;
 SELECT count(*) INTO N FROM CABLE_INSTALL_NODE WHERE CABLE=cab_seqid;
 IF N = 1 THEN
 SELECT NODE INTO InstallNode FROM CABLE_INSTALL_NODE WHERE CABLE=cab_seqid;
 END IF;
 FOR rec IN C1(cab_seqid) LOOP
 IF rec.nodid=InstallNode THEN
 Nodo := '*' || rec.node;
 ELSE
 Nodo := rec.node;
 END IF;
 Ruta := Ruta || Nodo || '^';
 END LOOP;
 Ruta := SUBSTR(Ruta, 0, LENGTH(Ruta) - 1);
 RETURN Ruta;
EXCEPTION
 WHEN OTHERS THEN
 RETURN Ruta;
end;
/
