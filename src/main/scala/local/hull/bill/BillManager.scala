package local.hull.bill

object BillManager {
//V_TOTAL_PROFILE_BILL
  case class PlateReadyForNest(
                                KSE: Int = 0,
                                mat: String = "",
                                w: String = "",
                                l: String = "",
                                t: String = "",
                                count: Int
                              )

  case class ProfileNestBill(
                              KSE: Int = 0,
                              KQ: String = "",
                              TP: String = "",
                              WH: Double = 0.0,
                              WT: Double = 0.0,
                              FH: Double = 0.0,
                              FT: Double = 0.0,
                              NESTID: String = "",
                              NP: Int = 0,
                              NGB: Int = 0,
                              NETLEN: Double = 0.0,
                              GROLEN: Double = 0.0,
                              TONETLEN: Double = 0.0,
                              TOGROLEN: Double = 0.0,
                              TONETWGT: Double = 0.0,
                              TOGROWGT: Double = 0.0,
                              SCRAP: Double = 0.0,
                              BLOCK: String = "",
                              BLOCK_OID: Int = 0,
                              TOTAL_BL_SCRAP: Double = 0.0,
                              TOTAL_KSE_SCRAP: Double = 0.0
                            )

  case class PlateNestBill(
                            KPL: Int = 0,
                            KQ: String = "",
                            L: Double = 0.0,
                            W: Double = 0.0,
                            T: Double = 0.0,
                            NESTID: String = "",
                            NP: Int = 0,
                            NGP: Int = 0,
                            TONETWGT: Double = 0.0,
                            TOGROWGT: Double = 0.0,
                            SCRAP: Double = 0.0,
                            BLOCK: String = "",
                            BLOCK_OID: Int = 0,
                            TOTAL_BL_SCRAP: Double = 0.0,
                            TOTAL_KPL_SCRAP: Double = 0.0,
                            ENCLOS_TYPE: Int = 0,
                            ASPECT_RATIO: Int = 0
                          )

}
