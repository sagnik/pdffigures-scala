package edu.ist.psu.sagnik.research.extractor



/**
 * Created by szr163 on 9/28/15.
 */

import java.io.File

import edu.ist.psu.sagnik.research.extractor.impl.{ PdfXtkModule, PdfxTkExtractor, PdfObjectPrettyPrint }
import edu.ist.psu.sagnik.research.extractor.captionmentionextraction.CaptionMention
import edu.ist.psu.sagnik.research.extractor.test.PdfXtkModuleTest
import org.scalatest.FunSpec
import org.json4s.native.JsonMethods._

case class AllenAIFigureCaption(Caption: Option[String])

class CaptionExtractionTest extends FunSpec {

  import PdfXtkModuleTest._

  describe("testing the pdfxtk output by printing") {

    it("should print the page number and bounding box etc.") {

      implicit val formats = org.json4s.DefaultFormats

      val extractedPdf=PdfXtkModule.Load(pdfloc).map(PdfxTkExtractor)

      val pdTextLines= extractedPdf.map(p=>p.pdTextLines).flatten

      val pdPargaraphs= extractedPdf.map(p=>p.pdParagraphs).flatten

      val pdGraphics = extractedPdf.map(p=>p.pdLines).flatten

      val pdImages= extractedPdf.map(p=>p.pdImages).flatten

      println(pdImages)

      CaptionMention.Load(pdTextLines,pdPargaraphs,pdGraphics,pdImages,List(20f).toIndexedSeq).foreach( //TODO: 20f is a placeholder for this function, actually we should pass x coordinates for col margins.
        p=>println(s"[type]: ${p.captionType}, [id]:${p.objectID} [content]:${p.content}" +
        s" [page number]: ${p.pageNumber} [isTextLine]: ${p.isTextLine}")
      )

      //TODO: 20f is a placeholder for this function, actually we should pass x coordinates for col margins.
      val captions=CaptionMention.Load(pdTextLines,pdPargaraphs,pdGraphics,pdImages,List(20f).toIndexedSeq).map(p=>(p.captionType,p.objectID,p.content,p.pageNumber))

      val captionPairs=captions.map(p => {
        val jsonLoc = pdfloc.substring(0, pdfloc.length - 4) + "-" + p._1 + "-" + p._2 + ".json"
        val allenAICaption = if (new File(jsonLoc).exists())
          parse(scala.io.Source.fromFile(jsonLoc).mkString).extract[AllenAIFigureCaption].Caption
        else
          None

        allenAICaption match {
          case Some(captionPresent) => (p._3.replaceAll("\\P{Alnum}", ""), captionPresent.replaceAll("\\P{Alnum}", ""))
          case None => ("None", "None")
        }
      })
      //captionPairs.foreach(a=>println(s"my caption: ${a._1} \n\npdffigure caption: ${a._2} \n-------------------------------------\n"))
      //print(jsonFiles)
    }
  }

}

