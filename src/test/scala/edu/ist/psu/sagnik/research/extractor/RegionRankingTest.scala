package edu.ist.psu.sagnik.research.extractor

/**
 * Created by szr163 on 10/20/15.
 */
import edu.ist.psu.sagnik.research.extractor.figuretableextraction.RegionRanking
import edu.ist.psu.sagnik.research.extractor.impl.{ PdfXtkModule, PdfxTkExtractor}
import edu.ist.psu.sagnik.research.extractor.test.PdfXtkModuleTest
import org.scalatest.FunSpec

class RegionRankingTest extends FunSpec {

  import PdfXtkModuleTest._

  describe("testing the pdfxtk output by printing") {

    it("should print the page number and bounding box etc.") {

      val extractedPdf=PdfXtkModule.Load(pdfloc).map(PdfxTkExtractor)
      val results=RegionRanking(extractedPdf,true)
      results.foreach(a=>println(s"[caption content]: ${a.caption.content} [page number]: ${a.caption.pageNumber} " +
        s"[regionbb]: ${a.figTableBB.bb} [region page number]: ${a.figTableBB.pageNumber}"))

    }
  }

}
