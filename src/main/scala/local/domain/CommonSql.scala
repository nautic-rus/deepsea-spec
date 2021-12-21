package local.domain

object CommonSql {
  val zonesAndSystemsSql="select\n'zone' as TYPENAME, \nOID,\nSEQID,\nUSERID,\nNAME,\n(select descr from ZONE_LANG where zone = oid and lang= -2) as DESCR_RU,\n(select descr from ZONE_LANG where zone = oid and lang= -1) as DESCR_EN\nfrom zone\nunion all\n\nselect \n'system' as TYPENAME,\nOID,\nSEQID,\nUSERID,\nNAME,\n(select descr from SYSTEMS_LANG where system = oid and lang= -2) as DESCR_RU,\n(select descr from SYSTEMS_LANG where system = oid and lang= -1) as DESCR_EN\nfrom systems"
}
