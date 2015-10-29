package edu.ist.psu.sagnik.research.extractor.writers

import edu.ist.psu.sagnik.research.extractor.model.{Rectangle, FigureCaption}
import org.json4s.JsonDSL._
import org.json4s.JsonDSL._
import org.json4s.jackson.JsonMethods._

import scala.reflect.io.File


/**
 * Created by sagnik on 10/29/15.
 */
object JSONWriter {

  def precReduce(d:Float):Double=BigDecimal(d).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble

  def precReduce(r:Rectangle):List[Double]=List(precReduce(r.x1),precReduce(r.y1),precReduce(r.x2),precReduce(r.x1))

  def caseClassToJsonString(fc:FigureCaption):String={
   val jsoncontent=
     (
       ("pageNumber" -> fc.caption.pageNumber)~
       ("captionContent"->fc.caption.content)~
         ("captionBB" -> precReduce(fc.caption.boundingBox))~
         ("figureBB" -> precReduce(fc.figure.figTableBB.bb))~
         ("figureWords" ->
           fc.figure.words.map { r =>
             ("wordBB"->precReduce(r.boundingBox))~
               ("wordContent"->r.content)
           }
           )
       )
    compact(render(jsoncontent))
  }
  def apply(fc:FigureCaption,loc:String):Unit={
    File(loc).writeAll(caseClassToJsonString(fc))
  }

}
