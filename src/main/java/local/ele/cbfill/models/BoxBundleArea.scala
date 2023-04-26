package local.ele.cbfill.models

import local.ele.cbfill.models.Cases.CBStuff

import scala.collection.mutable.ListBuffer

class BoxBundleArea(cableBoxModule: CableBoxModule, cBStuff: CBStuff) {

  var isFull = false
  var isError = false
  var isCurrentRowFull = false
  var isSpecialInsertion = false

  val localWaMatrix: ListBuffer[CableBundle] = ListBuffer.empty[CableBundle]
  val positionXmax: Int = cableBoxModule.unitL
  val positionYmax: Int = cableBoxModule.unitH

  var positionY: Int = 0
  var positionX: Int = 0

  var requestedModule: Option[SealModule] = None

  val sealModeules: List[SealModule] = cBStuff.sealModules

  var testcounter = 0

  def insertBundle(in: CableBundle, isTryToPackLast6rows: Boolean = false): Boolean = {
    isCurrentRowFull = false
    testcounter = testcounter + 1

    (positionXmax - (positionX + in.sealModule.unitL)) match {
      case x if x == 0 => {
        positionX = 0
        isCurrentRowFull = true
        positionY = positionY + in.sealModule.unitH
        if (positionY >= positionYmax || positionYmax - positionY <= 2) {
          isFull = true
        }
        if (isTryToPackLast6rows) {
          requestedModule = calculateNextModule()
        } else {
          requestedModule = None
        }

        localWaMatrix += in
        true
      }
      case x if x > 0 => {
        if ((positionYmax - positionY) >= in.sealModule.unitH) {
          requestedModule = Some(in.sealModule)
          positionX = positionX + in.sealModule.unitL
          localWaMatrix += in
          true
        } else {
          isFull = true
          false
        }
      }
      case x if x < 0 => {
        //M1.textArea.appendText(s"Check row x")
        false
      }

    }

  }

  def packLastRow(): Boolean = {
    if (requestedModule.isDefined && positionX != 0 && positionX < positionXmax) {
      val dummySeal: SealModule = requestedModule.get
      val cb = new CableBundle(None, dummySeal)
      insertBundle(cb)
    }
    if (positionX != 0) true else false
  }

  def insertWithException(in40: CableBundle, in41: CableBundle): Boolean = {
    localWaMatrix += in40
    localWaMatrix += in41
    requestedModule = None
    positionX = 0
    positionY = positionY + 8
    true
  }

  def qtyModules: Int = localWaMatrix.length

  private def lastInserted: (Int, Int) = {
    if (localWaMatrix.isEmpty) {
      (0, 0)
    } else {
      (localWaMatrix.last.sealModule.unitL, localWaMatrix.last.sealModule.unitH)
    }
  }

  private def calculateNextModule(): Option[SealModule] = {
    val elapsedRowsCount = positionYmax - positionY
    elapsedRowsCount match {
      case 6 => {
        if (lastInserted._1 > 3 && lastInserted._2 > 3) {
          val ret = sealModeules.find(p => p.unitL == 3)
          if (ret.isDefined) isSpecialInsertion = true
          ret
        } else {
          None
        }
      }

      case 3 => {
        if (lastInserted._1 == 3 && lastInserted._2 == 3 && isSpecialInsertion) {
          isSpecialInsertion = false
          sealModeules.find(p => p.unitL == 3)
        } else {
          None
        }
      }
      case _ => None
    }
  }

}
