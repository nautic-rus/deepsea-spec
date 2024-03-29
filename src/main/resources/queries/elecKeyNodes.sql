select
  nnd.seqid,
  nnd.userid,
  (
    select
      count(*)
    from
      (
        select
          cr.node1
        from
          cab_route cr
        where
          cr.node2 = nnd.seqid
        union
        select
          cr.node2
        from
          cab_route cr
        where
          cr.node1 = nnd.seqid
      ) cs
  ) as count
from
  node nnd
