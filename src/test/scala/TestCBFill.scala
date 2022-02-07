import local.ele.cbfill.models.Cases.{CBStuff, CableBoxCables}
import local.ele.cbfill.models.{BoxBundle, BoxDetailHelper, CableCBHelper}
import org.scalatest.funsuite.AnyFunSuite

import java.awt.image.BufferedImage
import java.io.File
import java.util.UUID
import javax.imageio.ImageIO
import scala.collection.mutable.ListBuffer

class TestCBFill extends AnyFunSuite with CableCBHelper with BoxDetailHelper {

  val foranCablesAll: List[CableBoxCables] = allCables("P701")


  private val totBundles = foranCablesAll.partition(p => p.foranCableBox.seal.equals("S") && (p.foranCableBox.cbxType == 2 || p.foranCableBox.cbxType == 1))

  val fillCableBoxes: List[CableBoxCables] = totBundles._1

  private val totBundles2 = totBundles._2.partition(p => p.foranCableBox.seal.equals("N") && p.foranCableBox.cbxType == 1)

  val sealsCableBoxes: List[CableBoxCables] = totBundles2._1

  val bundlesCableBoxes: List[CableBoxCables] = totBundles2._2

  val boxBundles: ListBuffer[BoxBundle] = ListBuffer.empty[BoxBundle]

  val boxBundlesErrors: ListBuffer[BoxBundle] = ListBuffer.empty[BoxBundle]

  val cBStuff: CBStuff = genCBStuff()

  initBoxBundles()
  def fillAll(): Unit = {
    boxBundles.filter(s => !s.isDone).foreach(bb => {
      //println(bb.foranCableBox.boxID)
      bb.pack()
    })
  }
  fillAll()

  val cc=boxBundles.head

  val jja=0

  boxBundles.foreach(bb=>{
   val bi: BufferedImage = bb.generateImage()
    val file1 = new File(s"c:\\20\\${ UUID.randomUUID().toString}.png")
    ImageIO.write(bi, "png", file1)
  })

  def initBoxBundles(): Unit = {
    bundlesCableBoxes.foreach(bc => {
      val boxBundle = new BoxBundle(bc, cBStuff)
      if (boxBundle.errorFlag) {
        boxBundlesErrors += boxBundle
      } else {
        boxBundles += boxBundle
      }
    })
  }

  def fillOne(in: CableBoxCables): Unit = {
    boxBundles.find(p => p.foranCableBox.boxID.equals(in.foranCableBox.boxID)) match {
      case Some(boxBundle) => {
        boxBundle.pack()
      }
      case None => None
    }
  }


  val jj = 0

}
