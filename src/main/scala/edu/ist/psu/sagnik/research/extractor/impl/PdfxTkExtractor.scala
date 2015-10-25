package edu.ist.psu.sagnik.research.extractor.impl

import at.ac.tuwien.dbai.pdfwrap.model.document._
import edu.ist.psu.sagnik.research.extractor.PdfModule
import edu.ist.psu.sagnik.research.extractor.model.Rectangle._
import edu.ist.psu.sagnik.research.extractor.model._

case object PdfxTkExtractor extends PdfModule[Page]#Extractor {

  import scala.collection.JavaConversions._

  override def apply(pdfxtkPageElements: Page) =
    pdfxtkPageElements.getItems
      .foldLeft(PdPageObject.empty) {

      case (pageObject@PdPageObject(bbs, lines, rects, rasters, chars, words, textLines, paragraphs), item) =>

        val r=pdfxtkPageElements.getBoundingBox //this gives the bounding box in the order x1,x2,y1,y2, while we
        //need them in the order x1,y1,x2,y2. Hence, r(0),r(2),r(1),r(3) and not r(0),r(1),r(2),r(3)

        val pNum = pdfxtkPageElements.getPageNo
        item.tagName() match {

          case "line-segment" =>
            val ls = item.asInstanceOf[LineSegment]
            val additionalLine =
              PdfLine(
                boundingBox = Rectangle.fromLineSegment(ls),
                pageNumber = pNum
              )
            pageObject.copy(
              pdLines = lines :+ additionalLine,
              pageBBs = bbs :+ Rectangle(r(0),r(2),r(1),r(3))
            )

          case "rect-segment" =>
            val rs = item.asInstanceOf[RectSegment]
            val additionalRect =
              PdfRect(
                boundingBox = Rectangle.fromRectSegment(rs),
                pageNumber = pNum
              )
            pageObject.copy(
              pdRects = rects :+ additionalRect,
              pageBBs = bbs :+ Rectangle(r(0),r(2),r(1),r(3))
            )

          case "image-segment" =>
            val is = item.asInstanceOf[ImageSegment]
            val additionalRaster =
              PdfRaster(
                boundingBox = Rectangle.fromImageSegment(is),
                pageNumber = pNum
              )
            pageObject.copy(
              pdImages = rasters :+ additionalRaster,
              pageBBs = bbs :+ Rectangle(r(0),r(2),r(1),r(3))
            )
          case "text-block" =>
            val tb = item.asInstanceOf[TextBlock]
            val additionalParagraph =
                PdfParagraph(
                  boundingBox = Rectangle.fromTextBlock(tb),
                  pageNumber = pNum,
                  content = tb.getText
                )

              val additionalTextLines =
                tb.getItems
                  .map(tl =>
                  PdfTextLine(
                    boundingBox = Rectangle.fromTextLine(tl),
                    pageNumber = pNum,
                    content = tl.getText
                  ))

              val additionalChars: Seq[PdfChar] =
                for {
                  tl <- tb.getItems
                  lf <- tl.getItems
                  tf <- lf.getItems
                  tc <- tf.getItems
                } yield {
                  PdfChar(
                    boundingBox = Rectangle.fromCharSegment(tc),
                    pageNumber = pNum,
                    content = tc.getText
                  )
                }

              val additionalWords = formWords(tb, pNum)

              pageObject.copy(
                pdChars = chars ++ additionalChars,
                pdWords = words ++ additionalWords,
                pdTextLines = textLines ++ additionalTextLines,
                pdParagraphs = if (additionalParagraph.content.trim.length>0) paragraphs :+ additionalParagraph else paragraphs,
                pageBBs = bbs :+ Rectangle(r(0), r(2), r(1), r(3))
              )

          case _ =>
            //System.err.println(s"[WARN] Skipping unrecognized item tagName: ${item.tagName()}")
            pageObject
        }
    }


  def formWords(tb: TextBlock, pageNumber: Int): Seq[PdfWord] =
    tb.getItems
      .map(x => textLineToWord(x, pageNumber))
      .flatten

  def textLineToWord(line: TextLine, pageNumber: Int): Seq[PdfWord] = {

    val lineChars: Seq[CharSegment] =
      for {
        lf <- line.getItems
        tf <- lf.getItems
        tc <- tf.getItems
      } yield tc

    val lineText = line.getText

    val indicesNonSpace =
      0 +: lineText.zipWithIndex.filter(_._1 == ' ').map(_._2) :+ lineText.length

    indicesNonSpace.sliding(2)
      .map { indexPair =>
      charSeqToWord(lineChars.slice(indexPair.head, indexPair(1)), pageNumber)
    }
      .flatten
      .toSeq
  }

  def charSeqToWord(chars: Seq[CharSegment], pageNumber: Int): Option[PdfWord] = {

    val content = chars.map(_.getText).mkString("")

    if (content.trim().length == 0)
      None

    else
      Some(
        PdfWord(
          boundingBox =
            Rectangle(
              chars.map(_.getX1).min,
              chars.map(_.getY1).min,
              chars.map(_.getX2).max,
              chars.map(_.getY2).max
            ),
          pageNumber = pageNumber,
          content = content
        )
      )
  }

}