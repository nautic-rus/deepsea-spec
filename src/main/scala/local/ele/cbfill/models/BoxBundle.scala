package local.ele.cbfill.models

import local.ele.cbfill.models.Cases._

import java.awt.{BasicStroke, Color, Graphics2D, RenderingHints}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.ListBuffer

class BoxBundle(cableBoxCables: CableBoxCables, cBStuff: CBStuff) {
  var isDone = false
  var errorFlag: Boolean = false
  private val errorMessages: ListBuffer[String] = ListBuffer.empty[String]

  val foranCableBox: ForanCableBox = cableBoxCables.foranCableBox
  val cables: List[ForanCable] = cableBoxCables.foranCables
  val cableBoxModule: Option[CableBoxModule] = findSuitableCableBox()

  private val breadthFirst: ListBuffer[BoxBundleArea] = cableBoxModule match {
    case Some(box) => {
      val buf = ListBuffer.empty[BoxBundleArea]
      (0 until box.columns).foreach(x => {
        buf += new BoxBundleArea(box, cBStuff)
      })
      buf
    }
    case None => ListBuffer.empty[BoxBundleArea]
  }

  private val depthFirst: ListBuffer[BoxBundleArea] = cableBoxModule match {
    case Some(box) => {
      val buf = ListBuffer.empty[BoxBundleArea]
      (0 until box.columns).foreach(x => {
        buf += new BoxBundleArea(box, cBStuff)
      })
      buf
    }
    case None => ListBuffer.empty[BoxBundleArea]
  }

  private val depthFirstExt: ListBuffer[BoxBundleArea] = cableBoxModule match {
    case Some(box) => {
      val buf = ListBuffer.empty[BoxBundleArea]
      (0 until box.columns).foreach(x => {
        buf += new BoxBundleArea(box, cBStuff)
      })
      buf
    }
    case None => ListBuffer.empty[BoxBundleArea]
  }

  private val cableBasketBF: ListBuffer[ForanCable] = {
    val ret = ListBuffer.empty[ForanCable]
    ret ++= cableBoxCables.foranCables.sortBy(p => p.oDiam).reverse
    ret
  }

  private val cableBasketDF: ListBuffer[ForanCable] = {
    val ret = ListBuffer.empty[ForanCable]
    ret ++= cableBoxCables.foranCables.sortBy(p => p.oDiam).reverse
    ret
  }

  private val cableBasketExt: ListBuffer[ForanCable] = {
    val ret = ListBuffer.empty[ForanCable]
    ret ++= cableBoxCables.foranCables.sortBy(p => p.oDiam).reverse
    ret
  }


  private def findSuitableCableBox(): Option[CableBoxModule] = {
    cBStuff.cableBoxModules.find(p => p.name.equals(foranCableBox.foranName)) match {
      case Some(value) => {
        Option(value)
      }
      case None => {
        errorFlag = true
        errorMessages += s"Can't find suitable CableBoxModule for name ${foranCableBox.boxID} ${foranCableBox.foranName}."
        //println(s"Can't find suitable CableBoxModule for name ${cablebox.foranCableBox.boxID} ${foranCableBox.foranName}.")
        None
      }
    }
  }

  def pack(): Unit = {
    cableBoxModule match {
      case Some(cbm) => {
        if (isDone) {
          //vis(breadthFirst, cbm, M1.sharpCanvas2.getGraphicsContext2D)
          //vis(depthFirst, cbm, M1.sharpCanvas.getGraphicsContext2D)
          //vis(depthFirstExt, cbm, M1.sharpCanvas3.getGraphicsContext2D)
          report()

        } else {
          if (cbm.columns == 1) {
            packBF(cbm)
            isDone = true
            report()
          } else {
            packBF(cbm)
            packDF(cbm)
            packBFExt(cbm)
            isDone = true
            report()
          }
        }
      }
      case None => None
    }
  }

  private def packDF(cbm: CableBoxModule): Unit = {
    var col = 0
    var count = 0
    while (cableBasketDF.nonEmpty && !errorFlag && !breadthFirst(col).isFull) {
      if (breadthFirst(col).requestedModule.isDefined && breadthFirst(col).requestedModule.get.unitL == 8 && cableBoxModule.get.unitL == 12) {
        val m1 = bundleBySizeeEx(4, 4, cableBasketDF)
        if (m1.foranCable.isDefined) cableBasketDF -= m1.foranCable.get
        val m2 = bundleBySizeeEx(4, 4, cableBasketDF)
        if (m2.foranCable.isDefined) cableBasketDF -= m2.foranCable.get
        breadthFirst(col).insertWithException(m1, m2)
      }
      else {
        breadthFirst(col).requestedModule match {
          case Some(seal) => {
            bundleBySize(seal.unitL, seal.unitH, cableBasketDF) match {
              case Some(bundle) => {
                if (breadthFirst(col).insertBundle(bundle)) {
                  bundle.foranCable match {
                    case Some(cable) => {
                      cableBasketDF -= cable
                    }
                    case None => {
                      None
                    }
                  }
                } else {
                  errorFlag = true
                }
              }
              case None => {
                errorFlag = true
              }
            }
          }
          case None => {
            bundleByMax(cableBasketDF) match {
              case Some(bundle) => {
                if (breadthFirst(col).insertBundle(bundle)) {
                  bundle.foranCable match {
                    case Some(cable) => {
                      cableBasketDF -= cable
                    }
                    case None => {
                      None
                    }
                  }
                }
              }
              case None => {
                None
              }
            }
          }
        }
        count = count + 1
        if (breadthFirst(col).isCurrentRowFull) {
          if (col >= cbm.columns - 1) col = 0 else col = col + 1
        }
      }
    }
    fillLastRow(cbm, breadthFirst)
    packEmptySpace(cbm, breadthFirst)
    //vis(breadthFirst, cbm, M1.sharpCanvas2.getGraphicsContext2D)
  }

  private def packBF(cbm: CableBoxModule): Unit = {
    (0 until cbm.columns).foreach(col => {
      while (cableBasketBF.nonEmpty && !errorFlag && !depthFirst(col).isFull) {
        if (depthFirst(col).requestedModule.isDefined && depthFirst(col).requestedModule.get.unitL == 8 && cableBoxModule.get.unitL == 12) {
          val m1 = bundleBySizeeEx(4, 4, cableBasketBF)
          if (m1.foranCable.isDefined) cableBasketBF -= m1.foranCable.get
          val m2 = bundleBySizeeEx(4, 4, cableBasketBF)
          if (m2.foranCable.isDefined) cableBasketBF -= m2.foranCable.get
          depthFirst(col).insertWithException(m1, m2)
        }
        else {
          depthFirst(col).requestedModule match {
            case Some(seal) => {
              bundleBySize(seal.unitL, seal.unitH, cableBasketBF) match {
                case Some(bundle) => {
                  if (depthFirst(col).insertBundle(bundle)) {
                    bundle.foranCable match {
                      case Some(cable) => {
                        cableBasketBF -= cable
                      }
                      case None => None
                    }
                  }
                }
                case None => {
                  errorFlag = true
                }
              }
            }
            case None => {
              bundleByMax(cableBasketBF) match {
                case Some(bundle) => {
                  val jj = 0
                  if (depthFirst(col).insertBundle(bundle)) {
                    bundle.foranCable match {
                      case Some(cable) => {
                        cableBasketBF -= cable
                      }
                      case None => None
                    }
                  }
                }
                case None => {
                  errorFlag = true
                }
              }
            }
          }
        }
      }
    })

    fillLastRow(cbm, depthFirst)
    packEmptySpace(cbm, depthFirst)
    //vis(depthFirst, cbm, M1.sharpCanvas.getGraphicsContext2D)
  }

  private def packBFExt(cbm: CableBoxModule): Unit = {

    (0 until cbm.columns).foreach(col => {
      while (cableBasketExt.nonEmpty && !errorFlag && !depthFirstExt(col).isFull) {

        if (depthFirstExt(col).requestedModule.isDefined && depthFirstExt(col).requestedModule.get.unitL == 8 && cableBoxModule.get.unitL == 12) {
          val m1 = bundleBySizeeEx(4, 4, cableBasketExt)
          if (m1.foranCable.isDefined) cableBasketExt -= m1.foranCable.get
          val m2 = bundleBySizeeEx(4, 4, cableBasketExt)
          if (m2.foranCable.isDefined) cableBasketExt -= m2.foranCable.get
          depthFirstExt(col).insertWithException(m1, m2)
        }
        else {
          depthFirstExt(col).requestedModule match {
            case Some(seal) => {
              bundleBySize(seal.unitL, seal.unitH, cableBasketExt) match {
                case Some(bundle) => {
                  if (depthFirstExt(col).insertBundle(bundle, true)) {
                    bundle.foranCable match {
                      case Some(cable) => {
                        cableBasketExt -= cable
                      }
                      case None => None
                    }
                  }
                }
                case None => {
                  errorFlag = true
                }
              }
            }
            case None => {
              bundleByMax(cableBasketExt) match {
                case Some(bundle) => {
                  if (depthFirstExt(col).insertBundle(bundle, true)) {
                    bundle.foranCable match {
                      case Some(cable) => {
                        cableBasketExt -= cable
                      }
                      case None => None
                    }
                  }
                }
                case None => {
                  errorFlag = true
                }
              }
            }
          }
        }
      }
    })


    fillLastRow(cbm, depthFirstExt)
    packEmptySpace(cbm, depthFirstExt)
    //vis(depthFirstExt, cbm, M1.sharpCanvas3.getGraphicsContext2D)
  }

  private def fillLastRow(cbm: CableBoxModule, in: ListBuffer[BoxBundleArea]): Unit = {
    if (!errorFlag) {
      (0 until cbm.columns).foreach(col => {
        if (in(col).requestedModule.isDefined && in(col).requestedModule.get.unitL == 8 && cableBoxModule.get.unitL == 12 && in(col).positionX != 0) {
          cBStuff.sealModules.find(p => p.unitH == 4 && p.unitL == 4) match {
            case Some(seal) => {
              val cb = new CableBundle(None, seal)
              in(col).insertWithException(cb, cb)
            }
            case None => None
          }
        }
        else {
          val s = 0
          while (in(col).packLastRow()) {
          }
        }
      })
    }
  }

  def packEmptySpace(cableBoxModule: CableBoxModule, in: ListBuffer[BoxBundleArea]): Unit = {
    (0 until cableBoxModule.columns).foreach(col => {

      if (cableBoxModule.unitH - in(col).positionY >= 18 && cableBoxModule.unitH - in(col).positionY < 24) {
        val r = 3
        val c = 6
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }

      if (cableBoxModule.unitH - in(col).positionY >= 24 && cableBoxModule.unitH - in(col).positionY < 30) {
        val r = 4
        val c = 6
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }
      if (cableBoxModule.unitH - in(col).positionY >= 30 && cableBoxModule.unitH - in(col).positionY < 36) {
        val r = 5
        val c = 6
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }

      if (cableBoxModule.unitH - in(col).positionY >= 36 && cableBoxModule.unitH - in(col).positionY < 42) {
        val r = 6
        val c = 6
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }

      if (cableBoxModule.unitH - in(col).positionY >= 42 && cableBoxModule.unitH - in(col).positionY < 48) {
        val r = 7
        val c = 6
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }

      if (cableBoxModule.unitH - in(col).positionY >= 48) {
        val r = 8
        val c = 6
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }

      val localRowUnitCount = in(col).positionY

      //println(localRowUnitCount + " " + (cableBoxModule.unitH - localRowUnitCount))


      if (cableBoxModule.unitH - in(col).positionY == 1) {
        val cb = bundleBySizeeEx(cableBoxModule.unitL, 1)
        in(col).insertBundle(cb)
      }
      else if (cableBoxModule.unitH - in(col).positionY == 2) {
        val cb = bundleBySizeeEx(cableBoxModule.unitL, 2)
        in(col).insertBundle(cb)
      }
      else if ((cableBoxModule.unitH - in(col).positionY) == 5) {
        val r = 1
        val c = 4
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        val cbOne = bundleBySizeeEx(cableBoxModule.unitL, 1)
        in(col).insertBundle(cbOne)
      }

      else if ((cableBoxModule.unitH - in(col).positionY) == 7) {
        val r = 1
        val c = 6
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        val cbOne = bundleBySizeeEx(cableBoxModule.unitL, 1)
        in(col).insertBundle(cbOne)
      }

      else if ((cableBoxModule.unitH - in(col).positionY) == 11) {
        var r = 2
        var c = 4
        var cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        r = 1
        c = 3
        cb = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }

      else if ((cableBoxModule.unitH - in(col).positionY) == 13) {
        val r = 2
        val c = 6
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        val cbOne = bundleBySizeeEx(cableBoxModule.unitL, 1)
        in(col).insertBundle(cbOne)
      }
      else if ((cableBoxModule.unitH - in(col).positionY) == 17) {
        var r = 1
        var c = 6
        var cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        r = 2
        c = 4
        cb = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        r = 1
        c = 3
        cb = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }

      else if ((cableBoxModule.unitH - in(col).positionY) == 19) {
        val r = 3
        val c = 6
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        val cbOne = bundleBySizeeEx(cableBoxModule.unitL, 1)
        in(col).insertBundle(cbOne)
      }
      else if ((cableBoxModule.unitH - in(col).positionY) == 10) {
        val r = 3
        val c = 3
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        val cbOne = bundleBySizeeEx(cableBoxModule.unitL, 1)
        in(col).insertBundle(cbOne)
      }
      else if ((cableBoxModule.unitH - in(col).positionY) == 14) {
        var r = 1
        var c = 6
        var cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        r = 2
        c = 4
        cb = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }
      else if ((cableBoxModule.unitH - in(col).positionY) == 15) {
        var r = 2
        var c = 6
        var cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        r = 1
        c = 3
        cb = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }
      else if ((cableBoxModule.unitH - in(col).positionY) == 9) {
        var r = 1
        var c = 6
        var cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        r = 1
        c = 3
        cb = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }
      else if ((cableBoxModule.unitH - in(col).positionY) == 16) {
        var r = 2
        var c = 6
        var cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
        r = 1
        c = 4
        cb = bundleBySizeeEx(c, c)
        (0 until r * cableBoxModule.unitL / c) foreach (i => in(col).insertBundle(cb))
      }


      // 5, 7, 11, 13, 17, 19, 23, 29, 31, 37
      else if ((cableBoxModule.unitH - in(col).positionY % 6) == 0) {
        val c = 6
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until cableBoxModule.unitL / c * (cableBoxModule.unitH - in(col).positionY) / c) foreach (i => in(col).insertBundle(cb))
      }
      else if ((cableBoxModule.unitH - in(col).positionY) % 4 == 0) {
        val c = 4
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until cableBoxModule.unitL / c * (cableBoxModule.unitH - in(col).positionY) / c) foreach (i => in(col).insertBundle(cb))
      }
      else if ((cableBoxModule.unitH - in(col).positionY) % 3 == 0) {
        val c = 3
        val cb: CableBundle = bundleBySizeeEx(c, c)
        (0 until cableBoxModule.unitL / c * (cableBoxModule.unitH - in(col).positionY) / c) foreach (i => in(col).insertBundle(cb))
      }

    })
  }

  private def bundleBySize(unitL: Int = 0, unitH: Int = 0, basket: ListBuffer[ForanCable]): Option[CableBundle] = {
    cBStuff.sealModules.find(p => p.unitH == unitH && p.unitL == unitL) match {
      case Some(module) => {
        val suitableCables = basket.filter(p => p.oDiam >= module.cabDMin && p.oDiam <= module.cabDMax).sortBy(d => d.oDiam).reverse
        if (suitableCables.isEmpty) {
          Option(new CableBundle(None, module))
        } else {
          Option(new CableBundle(Option(suitableCables.head), module))
        }
      }
      case None => {
        errorFlag = true
        None
      }
    }
  }

  private def bundleBySizeeEx(unitL: Int = 0, unitH: Int = 0, basket: ListBuffer[ForanCable] = ListBuffer.empty[ForanCable]): CableBundle = {
    cBStuff.sealModules.find(p => p.unitH == unitH && p.unitL == unitL) match {
      case Some(module) => {
        val suitableCables = basket.filter(p => p.oDiam >= module.cabDMin && p.oDiam <= module.cabDMax).sortBy(d => d.oDiam).reverse
        if (suitableCables.isEmpty) {
          new CableBundle(None, module)
        } else {
          new CableBundle(Option(suitableCables.head), module)
        }
      }
      case None => {
        errorFlag = true
        null
      }
    }
  }

  private def bundleByMax(basket: ListBuffer[ForanCable]): Option[CableBundle] = {
    if (basket.isEmpty) {
      None
    } else {
      val cableToBundle = basket.sortBy(s => s.oDiam).reverse.head
      val suitableModules: List[SealModule] = cBStuff.sealModules.filter(p => p.isFitCable(cableToBundle.oDiam))
      if (suitableModules.isEmpty) {
        None
      } else {
        val module = suitableModules.minBy(s => s.unitArea)
        Option(new CableBundle(Option(cableToBundle), module))
      }
    }
  }

  private def image(bundleAreas: List[BoxBundleArea], cbm: CableBoxModule, totBundles: List[CableBundle]): BufferedImage = {
    var offsetY: Int = 10
    var offsetX: Int = 10
    val scale = 5
    val imageWidth: Int = (cableBoxModule.get.unitL * cableBoxModule.get.k * cableBoxModule.get.columns * scale + offsetX * 2).toInt
    val imageHeight: Int = (cableBoxModule.get.unitH * scale * cableBoxModule.get.k + offsetY * 2).toInt
    val canvas: BufferedImage = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_RGB)
    val gc: Graphics2D = canvas.createGraphics()
    gc.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    gc.clearRect(0, 0, imageWidth, imageHeight)

    gc.setColor(Color.WHITE)
    gc.fillRect(0, 0, imageWidth, imageHeight)

    //gc.drawRect(0, 0, imageWidth, imageHeight)
    var exCounter = 0
    var moduleCounter = 1
    bundleAreas.foreach(bundleArea => {
      gc.setColor(Color.BLACK)
      gc.setStroke(new BasicStroke(1.0F))
      gc.drawRect(offsetX, offsetY, cbm.unitL * cbm.k * scale, cbm.unitH * cbm.k * scale)
      var posX: Int = offsetX
      var posY: Int = offsetY
      var rowY = 0
      bundleArea.localWaMatrix.foreach(ba => {
        if ((posX - offsetX) >= (cbm.unitL * cbm.k * scale)) {
          posX = offsetX
          posY = posY + rowY
        }
        val boxX = ba.sealModule.unitL * cbm.k * scale
        val boxY = ba.sealModule.unitH * cbm.k * scale
        val textScaleFactor: Float = {
          if (moduleCounter.toString.length < 3) .7f else .5f
        }
        ba.foranCable match {
          case Some(value) => {
            gc.setColor(Color.GREEN)
            //gc.setTextAlign(TextAlignment.CENTER)
            //gc.setTextBaseline(VPos.CENTER)
            //gc.setFont(new Font("Arial", boxY * textScaleFactor))
            gc.drawString(
              moduleCounter.toString,
              Math.round(posX + boxX - (boxX / 2)),
              Math.round(posY + boxY - (boxY / 2)),
            )
            gc.drawRect(posX, posY, boxX, boxY)
            moduleCounter = moduleCounter + 1
          }
          case None => {
            gc.setColor(Color.RED)
            gc.setStroke(new BasicStroke(1.0F))
            gc.drawRect(posX, posY, boxX, boxY)
            //gc.setTextAlign(TextAlignment.CENTER)
            //gc.setTextBaseline(VPos.CENTER)
            //gc.setFont(new Font("Arial", boxY * textScaleFactor))
            gc.drawString(
              moduleCounter.toString,
              Math.round(posX + boxX - (boxX / 2)),
              Math.round(posY + boxY - (boxY / 2)),
            )
            moduleCounter = moduleCounter + 1
          }
        }
        posX = posX + boxX
        rowY = boxY

        if (exCounter == 1) {
          posX = posX - boxX
          posY = posY + boxY
          exCounter = 0
        }

        if (ba.sealModule.unitL == 8 && cableBoxModule.get.unitL == 12) {
          exCounter = 1
        }
      })
      offsetX = offsetX + cbm.unitL * cbm.k * scale
    })
    gc.dispose()
    canvas
  }




  private def imageOld(bundleAreas: List[BoxBundleArea], cbm: CableBoxModule, totBundles: List[CableBundle]): BufferedImage = {
    var offsetY: Int = 10
    var offsetX: Int = 10
    val scale = 5
    val imageWidth: Int = (cableBoxModule.get.unitL * cableBoxModule.get.k * cableBoxModule.get.columns * scale + offsetX * 2).toInt
    val imageHeight: Int = (cableBoxModule.get.unitH * scale * cableBoxModule.get.k + offsetY * 2).toInt
    val canvas: BufferedImage = new BufferedImage(imageWidth, imageHeight, BufferedImage.TYPE_INT_RGB)
    val gc: Graphics2D = canvas.createGraphics()
    gc.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    gc.clearRect(0, 0, imageWidth, imageHeight)
    gc.drawRect(0, 0, imageWidth, imageHeight)
    var exCounter = 0
    var moduleCounter = 1
    bundleAreas.foreach(bundleArea => {
      gc.setColor(Color.BLACK)
      gc.setStroke(new BasicStroke(1.0F))
      gc.drawRect(offsetX, offsetY, cbm.unitL * cbm.k * scale, cbm.unitH * cbm.k * scale)
      var posX: Int = offsetX
      var posY: Int = offsetY
      var rowY = 0
      bundleArea.localWaMatrix.foreach(ba => {
        if ((posX - offsetX) >= (cbm.unitL * cbm.k * scale)) {
          posX = offsetX
          posY = posY + rowY
        }
        val boxX = ba.sealModule.unitL * cbm.k * scale
        val boxY = ba.sealModule.unitH * cbm.k * scale
        val textScaleFactor: Float = {
          if (moduleCounter.toString.length < 3) .7f else .5f
        }
        ba.foranCable match {
          case Some(value) => {
            gc.setColor(Color.GREEN)
            //gc.setTextAlign(TextAlignment.CENTER)
            //gc.setTextBaseline(VPos.CENTER)
            //gc.setFont(new Font("Arial", boxY * textScaleFactor))
            gc.drawString(
              moduleCounter.toString,
              Math.round(posX + boxX - (boxX / 2)),
              Math.round(posY + boxY - (boxY / 2)),
            )
            gc.drawRect(posX, posY, boxX, boxY)
            moduleCounter = moduleCounter + 1
          }
          case None => {
            gc.setColor(Color.RED)
            gc.setStroke(new BasicStroke(1.0F))
            gc.drawRect(posX, posY, boxX, boxY)
            //gc.setTextAlign(TextAlignment.CENTER)
            //gc.setTextBaseline(VPos.CENTER)
            //gc.setFont(new Font("Arial", boxY * textScaleFactor))
            gc.drawString(
              moduleCounter.toString,
              Math.round(posX + boxX - (boxX / 2)),
              Math.round(posY + boxY - (boxY / 2)),
            )
            moduleCounter = moduleCounter + 1
          }
        }
        posX = posX + boxX
        rowY = boxY

        if (exCounter == 1) {
          posX = posX - boxX
          posY = posY + boxY
          exCounter = 0
        }

        if (ba.sealModule.unitL == 8 && cableBoxModule.get.unitL == 12) {
          exCounter = 1
        }
      })
      offsetX = offsetX + cbm.unitL * cbm.k * scale
    })
    gc.dispose()
    // Save as PNG
    //val file1 = new File("c:\\14\\myimage.png")
    //ImageIO.write(canvas, "png", file1)
    // Save as JPEG
    //val file2 = new File("c:\\14\\myimage.jpg")
    //ImageIO.write(canvas, "jpg", file2)
    canvas
  }

  def isPacked: Boolean = cableBasketBF.isEmpty || cableBasketBF.isEmpty || cableBasketExt.isEmpty

  private def report(): Unit = {

  }


  def isCablePacked(in: ForanCable): Boolean = {
    val cNundles = ListBuffer.empty[CableBundle]

    val b: List[BoxBundleArea] = fitBoxBundleArea()
    b.foreach(ba => {
      cNundles ++= ba.localWaMatrix
    })
    if (cNundles.toList.exists(p => p.foranCable.isDefined && p.foranCable.get == in)) true else false
    //if( cNundles.toList.exists(p => p.localWaMatrix.toList.exists(p =>p.foranCable.isDefined && p.foranCable.get == in))) true else false
  }

  def isCablePackedByAlgo1(in: ForanCable): Boolean = {
    val cNundles = ListBuffer.empty[CableBundle]
    val b: List[BoxBundleArea] = depthFirst.toList
    b.foreach(ba => {
      cNundles ++= ba.localWaMatrix
    })
    if (cNundles.toList.exists(p => p.foranCable.isDefined && p.foranCable.get == in)) true else false
  }

  def isCablePackedByAlgo2(in: ForanCable): Boolean = {
    val cNundles = ListBuffer.empty[CableBundle]
    val b: List[BoxBundleArea] = breadthFirst.toList
    b.foreach(ba => {
      cNundles ++= ba.localWaMatrix
    })
    if (cNundles.toList.exists(p => p.foranCable.isDefined && p.foranCable.get == in)) true else false
  }

  def isCablePackedByAlgo3(in: ForanCable): Boolean = {
    val cNundles = ListBuffer.empty[CableBundle]
    val b: List[BoxBundleArea] = depthFirstExt.toList
    b.foreach(ba => {
      cNundles ++= ba.localWaMatrix
    })
    if (cNundles.toList.exists(p => p.foranCable.isDefined && p.foranCable.get == in)) true else false
  }

  private def fitBoxBundleArea(): List[BoxBundleArea] = {
    case class TEST(name: String, content: List[BoxBundleArea], usedPercent: Int)
    val tmp = ListBuffer.empty[TEST]

    if (cableBasketBF.isEmpty) {
      tmp += TEST("DP", depthFirst.toList, usedPercent(depthFirst.toList))
    }
    if (cableBasketDF.isEmpty) {
      tmp += TEST("BF", breadthFirst.toList, usedPercent(breadthFirst.toList))
    }
    if (cableBasketExt.isEmpty) {
      tmp += TEST("DPE", depthFirstExt.toList, usedPercent(depthFirstExt.toList))
    }

    if (tmp.nonEmpty) {
      tmp.minBy(s => s.usedPercent).content
    } else {
      depthFirst.toList
    }

  }

  def qtyModules(): Int = {
    val in = fitBoxBundleArea()
    var tot = 0
    in.foreach(module => {
      tot = tot + module.qtyModules
    })
    tot
  }

  def totBundles(): List[CableBundle] = {
    val in = fitBoxBundleArea()
    val ret = ListBuffer.empty[CableBundle]
    in.foreach(b => {
      ret ++= b.localWaMatrix
    })
    ret.toList
  }

  def usedPercent(): Int = {
    val in = fitBoxBundleArea()
    if (!errorFlag) {
      val lb = ListBuffer.empty[CableBundle]
      in.foreach(ba => {
        lb ++= ba.localWaMatrix
      })
      val usedArea: Int = lb.filter(p => p.foranCable.isDefined).map(_.sealModule.unitArea).sum
      val totArea: Int = cableBoxModule.get.unitArea * cableBoxModule.get.columns
      val ret = Math.round((usedArea.toDouble / totArea.toDouble) * 100.0)
      ret.toInt
    } else {
      1000
    }
  }

  def usedPercent(in: List[BoxBundleArea]): Int = {
    if (!errorFlag) {
      val lb = ListBuffer.empty[CableBundle]
      in.foreach(ba => {
        lb ++= ba.localWaMatrix
      })
      val usedArea: Int = lb.filter(p => p.foranCable.isDefined || (p.sealModule.cabDMax == 0 && p.sealModule.cabDMin == 0)).map(_.sealModule.unitArea).sum
      val totArea: Int = cableBoxModule.get.unitArea * cableBoxModule.get.columns
      val ret = Math.round((usedArea.toDouble / totArea.toDouble) * 100.0)
      ret.toInt
    } else {
      0
    }
  }

  def generateImage(): BufferedImage = {
    if (!errorFlag) {
      image(fitBoxBundleArea(), cableBoxModule.get, totBundles())
    } else {
      image(fitBoxBundleArea(), cableBoxModule.get, totBundles())
    }
  }

  def calculateCompressionBlocks(): List[CompressionBlock] = {
    val ret = ListBuffer.empty[CompressionBlock]
    val cb = cBStuff.compressionBlocks.find(p => p.L == cableBoxModule.get.L)
    if (cb.isDefined) {
      (0 until cableBoxModule.get.columns).foreach(i => {
        if (cb.isDefined) ret += cb.get
      })
    }
    ret.toList
  }

  def calculateAnkerPlates(): List[AnkerPlate] = {
    val bundleAreas = fitBoxBundleArea()
    val ret = ListBuffer.empty[AnkerPlate] // ankerPlates
    val cbm = cableBoxModule.get
    cBStuff.ankerPlates.find(p => p.unitL == cableBoxModule.get.unitL) match {
      case Some(ap) => {
        val scale = 5
        var exCounter = 0
        var moduleCounter = 1
        bundleAreas.foreach(bundleArea => {
          var posX: Int = 0
          var posY: Int = 0
          var rowY = 0

          bundleArea.localWaMatrix.foreach(ba => {
            if ((posX) >= (cbm.unitL * cbm.k * scale)) {
              posX = 0
              posY = posY + rowY
              ret += ap
            }
            val boxX = ba.sealModule.unitL * cbm.k * scale
            val boxY = ba.sealModule.unitH * cbm.k * scale

            moduleCounter = moduleCounter + 1

            posX = posX + boxX
            rowY = boxY

            if (exCounter == 1) {
              posX = posX - boxX
              posY = posY + boxY
              exCounter = 0
              ret += ap
            }
            if (ba.sealModule.unitL == 8 && cableBoxModule.get.unitL == 12) {
              exCounter = 1
            }
          })
        })

        bundleAreas.foreach(ba => {
          ret += ap
        })

      }
      case None => None
    }


    ret.toList
  }
}
