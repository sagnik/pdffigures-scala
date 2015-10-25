package edu.ist.psu.sagnik.research.extractor

/**
 * Created by szr163 on 10/19/15.
 */

import edu.ist.psu.sagnik.research.extractor.figuretableextraction.RegionExtension
import edu.ist.psu.sagnik.research.extractor.impl.{ PdfXtkModule, PdfxTkExtractor}
import edu.ist.psu.sagnik.research.extractor.test.PdfXtkModuleTest
import org.scalatest.FunSpec

class RegionExtensionTest extends FunSpec {

  import PdfXtkModuleTest._

  describe("testing the pdfxtk output by printing") {

    it("should print the page number and bounding box etc.") {

      val extractedPdf=PdfXtkModule.Load(pdfloc).map(PdfxTkExtractor)
      val a=RegionExtension(extractedPdf)

    }
  }

}
