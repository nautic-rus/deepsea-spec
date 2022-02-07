package local.ele.cbfill.models

import local.ele.cbfill.models.Cases.ForanCable

class CableBundle(val foranCable: Option[ForanCable], val sealModule: SealModule) {

  override def toString: String = foranCable match {
    case Some(c)=>c.cableIndex+" "+sealModule.unitL.toString +"x"+sealModule.unitH.toString
    case None=>sealModule.unitL.toString +"x"+sealModule.unitH.toString
  }


  def toStringWithFormat:String={
    toString+" "+sealModule.unitH.toString+"/"+sealModule.unitL.toString
  }
}
