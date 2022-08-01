import breeze.linalg.{DenseVector, cross}
import org.scalatest.funsuite.AnyFunSuite

class SymplyTest extends AnyFunSuite {

  case class Point(x:Double,y:Double)
  val P1: DenseVector[Double] = DenseVector[Double](10.0, 10.0)
  val Q1: DenseVector[Double] = DenseVector[Double](80.0, 20.0)

  val P2: DenseVector[Double] = DenseVector[Double](30.0, 40.0)
  val Q2: DenseVector[Double] = DenseVector[Double](70.0, 10.0)




/*
  val A: DenseVector[Double] = DenseVector[Double](a2P(0) - a1P(0), a2P(1) - a1P(1))
  val B: DenseVector[Double] = DenseVector[Double](b2P(0) - b1P(0), b2P(1) - b1P(1))
*/


  /*
  Q=a1
  P=a2
  val a = Q.y - P.y;
  val b = P.x - Q.x;
  val c = a * (P.x) + b * (P.y);
    */

  val a1: Double = Q1(1) - P1(1)
  val b1: Double = P1(0) - Q1(0)
  val c1: Double = (a1 * P1(0) + b1 * P1(1))

  val a2: Double = Q2(1) - P2(1)
  val b2: Double = P2(0) - Q2(0)
  val c2: Double = (a1 * P2(0) + b1 * P2(1))


  val x = ((b1 * c2) - (b2 * c1) / (a1 * b2) - (a2 - b1));
  val y = ((a2 * c1) - (a1 * c2) / (a1 * b2) - (a2 * b1));




  val i=inter(
    Point(P1(0),P1(1)),
    Point(Q1(0),Q1(1)),

    Point(P2(0), P2(1)),
    Point(Q2(0), Q2(1)),
  )


  println(i.x+ " "+i.y)

  val k = 0

  def inter ( s1:Point, s2:Point,  d1:Point,  d2:Point) :Point={
//s1 and s2 are the endpoints of the first line and d1 and d2 are the endpoints of the second line
    val a1 = s2.y - s1.y
    val b1 = s1.x - s2.x
    val c1 = a1 * s1.x + b1 * s1.y

    val a2 = d2.y - d1.y
    val b2 = d1.x - d2.x
    val c2 = a2 * d1.x + b2 * d1.y

    val delta = a1 * b2 - a2 * b1
    val x=((b2 * c1 - b1 * c2) / delta)
    val y=( (a1 * c2 - a2 * c1) / delta)

    Point(x,y)
  }


}
