package edu.ist.psu.sagnik.research.extractor.figuretableextraction

/**
 * Created by szr163 on 10/3/15.
 */

import java.io.File

object DataLocation {

  import scala.util.matching.Regex
  def apply(f: File, r: Regex): Array[File] = {
    val these = f.listFiles
    val good = these.filter(f => r.findFirstIn(f.getName).isDefined)
    good ++ these.filter(_.isDirectory).flatMap(apply(_,r))
  }
}