package local

import local.common.DB

object M1 extends DB{
  def main(args: Array[String]): Unit = {

    oracleConnection() match {
      case Some(c) => c.close()
      case None => None
    }

  }
}
