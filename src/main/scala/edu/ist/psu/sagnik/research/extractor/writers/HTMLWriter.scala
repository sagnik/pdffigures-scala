package edu.ist.psu.sagnik.research.extractor.writers

import java.io.File
import java.io.File

import edu.ist.psu.sagnik.research.extractor.figuretableextraction.DataLocation


/**
 * Created by szr163 on 10/24/15.
 */
object HTMLWriter {
  def apply(dirName:String, htmlLoc:String):Unit={
    val figTableSVGs=DataLocation(new File(dirName),"-(Figure||Table)\\d+.svg"r)
    val htmlHeaderString="<!DOCTYPE html>\n<html>\n" +
      "<style>" +
      "\nh1 {color:red;}\np {color:blue;}\n" +
      "</style>\n" +
      "<body>\n" +
      "<h1>" +
      dirName+
      "</h1>\n"
    val htmlFooterString="</body>\n</html>"

    import scala.reflect.io.File

    File(htmlLoc).writeAll(
      htmlHeaderString+
        figTableSVGs.foldLeft("")(
          (a,b) => a+
            "<p><img style=\"border:5px solid blue\"" +
            "src=\"" +
            b.getAbsolutePath +
            "\">" +
            "</p>\n"
        )+
        htmlFooterString
    )
  }
}
