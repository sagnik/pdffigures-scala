package edu.ist.psu.sagnik.research.extractor.writers

import edu.ist.psu.sagnik.research.extractor.model.CaptionWithFigTableSVG
import scala.reflect.io.File

/**
 * Created by szr163 on 10/24/15.
 */
object SVGWriter {
  def apply(fc:CaptionWithFigTableSVG,loc:String):Unit={
    //println("in svg writer"+loc)

    val figBB=fc.figTableBB.bb
    val svgStrings=fc.svgPaths.map(x=>x.pathContent) //TODO: This has to be modified.

    val svgHeader="<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n <svg width=\"" +
      (figBB.x2-figBB.x1).toString +
      "\" height=\"" +
      (figBB.y2-figBB.y1).toString +
      "\" stroke=\"none\" stroke-width=\"0.0\" xmlns=\"http://www.w3.org/2000/svg\" xmlns:svgx=\"http://www.xml-cml.org/schema/svgx\">"

    val svgCloser="</svg>"

    val xTranslate=(figBB.x1*(-1)).toString
    val yTranslate=(figBB.y1*(-1)).toString
    val charTranslatePair=("<text ","<text transform=\"translate("+xTranslate+","+yTranslate+")\" ")
    val pathTranslatePair=("<path ","<path transform=\"translate("+xTranslate+","+yTranslate+")\" ")
    val imageTranslatePair=("<image ","<image transform=\"translate("+xTranslate+","+yTranslate+")\" ")

    val transformTranslatePair=("transform=\"", "transform=\"translate("+xTranslate+","+yTranslate+") ")

    File(loc).writeAll(
      svgHeader+"\n"+
      svgStrings.foldLeft("")((a,b)=>
        if (b.contains("transform")) a+b.replaceAll(transformTranslatePair._1,transformTranslatePair._2)+"\n"
        else
        a+ b.replaceAll(charTranslatePair._1,charTranslatePair._2)
      .replaceAll(pathTranslatePair._1,pathTranslatePair._2)
      .replaceAll(imageTranslatePair._1,imageTranslatePair._2)
      +"\n")+
      svgCloser

    )
  }
}
