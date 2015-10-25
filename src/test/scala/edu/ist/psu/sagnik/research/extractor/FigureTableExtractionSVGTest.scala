package edu.ist.psu.sagnik.research.extractor

import edu.ist.psu.sagnik.research.extractor.figuretableextraction.{FigureTableExtractionSVG}
import edu.ist.psu.sagnik.research.extractor.impl.{PdfxTkExtractor, PdfXtkModule}
import edu.ist.psu.sagnik.research.extractor.test.PdfXtkModuleTest
import org.scalatest.FunSpec

/**
 * Created by szr163 on 10/22/15.
 */
class FigureTableExtractionSVGTest extends FunSpec {

  import PdfXtkModuleTest._

  describe("testing the pdfxtk output by printing") {

    it("should print the page number and bounding box etc.") {
      val extractedPdf=PdfXtkModule.Load(pdfloc).map(PdfxTkExtractor)
      FigureTableExtractionSVG(extractedPdf,pdfloc)
    }
  }

}


