

import scala.collection.immutable._

/**
  * https://www.codingame.com/training/easy/mime-type
  * Created by kotobotov.ru on 06.03.2017.
  */

object Solution extends App {
  val n = readInt
  // Number of elements which make up the association table.
  val q = readInt
  // Number Q of file names to be analyzed.
  val mimeTable: scala.collection.mutable.HashMap[String, String] = scala.collection.mutable.HashMap.empty[String, String]
  var answer = List.empty[String]
  for (i <- 0 until n) {
    // ext: file extension
    // mt: MIME type.

    val Array(ext, mt) = readLine split " "
    mimeTable += (ext.toLowerCase -> mt)
    Console.err.println(ext + " -> " + mt)
  }
  for (i <- 0 until q) {
    val fname = readLine // One file name per line.
    fname.contains(".") match {
      case false => println("UNKNOWN")
      case true =>
        val otvet = try {
          val extention = fname.substring(fname.lastIndexOf(".") + 1, fname.size)
          mimeTable.getOrElse(extention.toLowerCase, "UNKNOWN")
        } catch {
          case e: Exception => "UNKNOWN"
        }
        println(otvet)
    }

  }
}