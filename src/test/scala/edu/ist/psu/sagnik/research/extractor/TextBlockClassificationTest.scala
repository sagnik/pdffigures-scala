package edu.ist.psu.sagnik.research.extractor

/**
 * Created by szr163 on 10/8/15.
 */
import java.io.File

import edu.ist.psu.sagnik.research.extractor.FigureTableExtraction.TextBlockClassification
import edu.ist.psu.sagnik.research.extractor.impl.{ PdfXtkModule, PdfxTkExtractor, PdfObjectPrettyPrint }
import edu.ist.psu.sagnik.research.extractor.test.PdfXtkModuleTest
import org.scalatest.FunSpec

class TextBlockClassificationTest extends FunSpec {

  import PdfXtkModuleTest._

  describe("testing the pdfxtk output by printing") {

    it("should print the page number and bounding box etc.") {

      implicit val formats = org.json4s.DefaultFormats

      val extractedPdf=PdfXtkModule.Load(pdfloc).map(PdfxTkExtractor)
      val a=TextBlockClassification(extractedPdf)

    }
  }

}
