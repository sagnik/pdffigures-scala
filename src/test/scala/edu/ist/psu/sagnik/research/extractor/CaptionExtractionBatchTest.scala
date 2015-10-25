package edu.ist.psu.sagnik.research.extractor



/**
 * Created by szr163 on 9/28/15.
 */

import edu.ist.psu.sagnik.research.extractor.impl.{ PdfXtkModule, PdfxTkExtractor, PdfObjectPrettyPrint }
import edu.ist.psu.sagnik.research.extractor.captionmentionextraction.CaptionMention
import edu.ist.psu.sagnik.research.extractor.test.PdfXtkModuleTest
import org.scalatest.FunSpec

import org.json4s.native.JsonMethods._
import java.io.File
import PdfXtkModuleTest._

class CaptionExtractionBatchTest extends FunSpec {

  describe("testing the pdfxtk output by printing") {
    import edu.ist.psu.sagnik.research.extractor.test.PdfXtkModuleTest._
    it("should check if the ALLENAI output and our outputs are same.") {

      val pdFiles = DataLocation(new File(dirloc), "pdf$" r)

      pdFiles.foreach(a =>
        scala.tools.nsc.io.File("verboseresults3").appendAll(
          s"checking ${a.getCanonicalPath} \n----------------\n" +
            s"${checkOne(a.getCanonicalPath)}\n----------------\n"
        )
      )

      /*val results=pdFiles.map(a=>checkOneFile(a.getAbsolutePath))
      results.zipWithIndex.foreach(a=>println(s"checked ${pdFiles(a._2).getCanonicalPath}, " +
        s"matched ${a._1._1} captions with ${a._1._2} captions"))*/
    }
  }

  def checkOneFile(pdfLoc: String): (Int, Int) = {
    println(s"processing ${pdfLoc}")
    implicit val formats = org.json4s.DefaultFormats
    try {
      val extractedPdf = PdfXtkModule.Load(pdfLoc).map(PdfxTkExtractor)

      val pdTextLines= extractedPdf.flatMap(p=>p.pdTextLines)

      val pdPargaraphs= extractedPdf.flatMap(p=>p.pdParagraphs)

      val pdGraphics = extractedPdf.flatMap(p=>p.pdLines)

      val pdImages= extractedPdf.flatMap(p=>p.pdImages)

      //TODO: 20f is a placeholder for this function, actually we should pass x coordinates for col margins.
      val captions = CaptionMention.Load(pdTextLines, pdPargaraphs,pdGraphics,pdImages,List(20f).toIndexedSeq).map(p => (p.captionType, p.objectID, p.content, p.pageNumber))
      val captionPairs = captions.map(p => {
        val jsonLoc = pdfLoc.substring(0, pdfLoc.length - 4) + "-" + p._1 + "-" + p._2 + ".json"
        val allenAICaption = if (new File(jsonLoc).exists())
          parse(scala.io.Source.fromFile(jsonLoc).mkString).extract[AllenAIFigureCaption].Caption
        else
          None
        allenAICaption match {
          case Some(captionPresent) => (p._3.replaceAll("\\P{Alnum}", ""), captionPresent.replaceAll("\\P{Alnum}", ""))
          case None => ("None", "None")
        }
      })
      implicit def bool2int(b: Boolean) = if (b) 1 else 0
      (captionPairs.foldLeft(0)((a, b) => a + b._1.equals(b._2)) + captionPairs.filter(a => a._1.equals("None")).length,
        captionPairs.length - captionPairs.filter(a => a._2.equals("None")).length)
    } catch {
      case e: java.lang.IllegalArgumentException => {
        println(s"Exception $pdfLoc")
        (0, DataLocation(new File(dirloc), pdfLoc.substring(0, pdfLoc.length - 4) r).length - 1)
      }
    }
  }
    def checkOne(pdfLoc: String): Seq[(String, String, Boolean)] = {
      println(s"processing ${pdfLoc}")
      implicit val formats = org.json4s.DefaultFormats
      try {
        val extractedPdf = PdfXtkModule.Load(pdfLoc).map(PdfxTkExtractor)

        val pdTextLines= extractedPdf.flatMap(p=>p.pdTextLines)

        val pdPargaraphs= extractedPdf.flatMap(p=>p.pdParagraphs)

        val pdGraphics = extractedPdf.flatMap(p=>p.pdLines)

        val pdImages= extractedPdf.flatMap(p=>p.pdImages)

        //TODO: 20f is a placeholder for this function, actually we should pass x coordinates for col margins.
        val captions = CaptionMention.Load(pdTextLines, pdPargaraphs, pdGraphics, pdImages,List(20f).toIndexedSeq).map(p => (p.captionType, p.objectID, p.content, p.pageNumber, p.ID))
        val captionPairs = captions.map(p => {
          val jsonLoc = pdfLoc.substring(0, pdfLoc.length - 4) + "-" + p._1 + "-" + p._2 + ".json"
          val allenAICaption = if (new File(jsonLoc).exists())
            parse(scala.io.Source.fromFile(jsonLoc).mkString).extract[AllenAIFigureCaption].Caption
          else
            None
          allenAICaption match {
            case Some(captionPresent) => (p._3.replaceAll("\\P{Alnum}", "")+"\n", captionPresent.replaceAll("\\P{Alnum}", ""),
              p._3.replaceAll("\\P{Alnum}", "").equals( captionPresent.replaceAll("\\P{Alnum}", "")))
            case None => (p._3, "None", false)
          }
        })
        captionPairs
      } catch {
        case e: java.lang.IllegalArgumentException => {
          println(s"Exception $pdfLoc")
          List(("my error", "my error", false)).toIndexedSeq
        }
      }

    }

}


