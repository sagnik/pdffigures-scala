package edu.ist.psu.sagnik.research.extractor.test

import edu.ist.psu.sagnik.research.extractor.impl.{ PdfXtkModule, PdfxTkExtractor, PdfObjectPrettyPrint }
import org.scalatest.FunSpec

class PdfXtkModuleTest extends FunSpec {

  import PdfXtkModuleTest._

  describe("testing the pdfxtk output by printing") {

    it("should print the page number and bounding box etc.") {
      PdfObjectPrettyPrint(PdfXtkModule.Load(pdfloc).map(PdfxTkExtractor))
    }
  }

}

object PdfXtkModuleTest {

  val pdfloc = "src/test/resources/pdfs/pdffigures.pdf"
  //val pdfloc="/home/szr163/pdffigures-scala/src/test/resources/longpdfs/10.1.1.151.6505.pdf"
  //val pdfloc="/home/szr163/pdffigures-scala/src/test/resources/longpdfs/10.1.1.362.5763.pdf"
  // val pdfloc="/home/szr163/pdffigures-scala/src/test/resources/longpdfs/10.1.1.72.4202.pdf"
  //val pdfloc="/home/szr163/pdffigures-scala/src/test/resources/longpdfs/10.1.1.374.7899.pdf"
  //val pdfloc="/home/szr163/pdffigures-scala/src/test/resources/longpdfs/10.1.1.367.6218.pdf"
  //val pdfloc="/home/szr163/pdffigures-scala/src/test/resources/longpdfs/10.1.1.77.9158.pdf"
  //val pdfloc="/home/szr163/pdffigures-scala/src/test/resources/longpdfs/10.1.1.61.1137.pdf"
  //val pdfloc="/home/szr163/pdffigures-scala/src/test/resources/longpdfs/10.1.1.421.8611.pdf"
  val dirloc= "src/test/resources/longpdfs"
  //val dirloc="paper"
}