package edu.ist.psu.sagnik.research.extractor.model



/**
 * Abstract data type representing an object appearing on a PDF page.
 */
sealed trait GenericPdObject {
  def boundingBox: Rectangle
  def pageNumber: Int
}

/**
 * A graphics path that is a straight line or a curve. We output the bounding box for the curve.
 */
case class PdfLine(boundingBox: Rectangle, pageNumber: Int) extends GenericPdObject

/**
 * A line that acts as a header or footer dividing line.
 */

case class HeaderFooterLine(boundingBox: Rectangle, pageNumber: Int, isTop:Boolean) extends GenericPdObject

/**
 * A graphics path that is not a line: some other shape that is contained
 * within the bounding box.
 */
case class PdfRect(boundingBox: Rectangle, pageNumber: Int) extends GenericPdObject

/**
 * An image.
 */
case class PdfRaster(boundingBox: Rectangle, pageNumber: Int) extends GenericPdObject

/**
 * An individual character. The `content` field will always be only one character.
 */
case class PdfChar(boundingBox: Rectangle, pageNumber: Int, content: String) extends GenericPdObject

/**
 * An entire word. This is a best-guess attempt. It is not guarenteed to correspond
 * 1-to-1 with the actual words that a human being would determine exist on a PDF page.
 */
case class PdfWord(boundingBox: Rectangle, pageNumber: Int, content: String) extends GenericPdObject

/**
 * A line of text from a PDF page.
 */
case class PdfTextLine(boundingBox: Rectangle, pageNumber: Int, content: String) extends GenericPdObject

/**
 * A contiguous section of text. May be multi-line.
 */
case class PdfParagraph(boundingBox: Rectangle, pageNumber: Int, content: String) extends GenericPdObject

/**
 * A Figure/table caption/mention. A paragraph or a line.
 */

case class Caption(boundingBox: Rectangle, pageNumber: Int, content: String, objectID:String, captionType:String,
                    ID:String, confidence:Float, isTextLine:Boolean,
                    colNo:Int, spansMultiColumn:Boolean) extends GenericPdObject

case class Mention(boundingBox: Rectangle, pageNumber: Int, content: String, objectID:String, mentionType:String) extends GenericPdObject

/**
 * A TextBlock can be a PDfTextLine or a PdfParagraph. Ideally, every text line should be part of a PDF paragraph. But due to some problems
 * in the PdfXtk module, there are some text lines that are NOT part of a paragraph. All text paragraphs (and text lines that are not a part of a
 * paragraph is a PdfTextBlock. Each PdfTextBlock also has a columnNo. For single column documents this value will be 1.  Note, for multi column documents,
 * there can be text blocks spanning multiple columns. They CAN have wrong values, but that won't affect our final goal much, which is to classify
 * each textblock as a caption text block (part of caption)/ body text block (part of the main body/header/footer) / figure or table text block (part of a figure/table)
 * We also assume that documents are either three column/ two column or single column.
 * NOTE these assumptions are only valid for well formatted scholarly papers.
 */

case class PdfTextBlock(boundingBox: Rectangle, pageNumber: Int, content: String, columnNo:Int) extends GenericPdObject

case class CaptionTextBlock(boundingBox: Rectangle, pageNumber: Int, content: String, columnNo:Int) extends GenericPdObject

case class BodyTextBlock(boundingBox: Rectangle, pageNumber: Int, content: String, columnNo:Int) extends GenericPdObject

case class FigTableTextBlock(boundingBox: Rectangle, pageNumber: Int, content: String, columnNo:Int) extends GenericPdObject

case class ClassifiedTextBlocks(captionTbs:Seq[CaptionTextBlock], bodyTbs:Seq[BodyTextBlock], figTableTbs:Seq[FigTableTextBlock])


/**
 * An intermediate object for each caption.
 * We also have a tuple of four rectangles for each caption, left, right, top and down. Imagine each caption as a pixel in a grid.
 * These are Option values, can be none as well.
 */
case class Region(bb:Rectangle,pageNumber:Int)
case class AdjRegions(left:Option[Region], down:Option[Region], right:Option[Region], top:Option[Region])

case class CaptionAdjRegion(caption:Caption, adjRegions:AdjRegions)
case class CaptionWithRegions(caption:Caption, regions:Seq[Region])

/**
 * Penaltimate class where a caption is stored with a the bounding box for a figure or a table.
 */

case class CaptionWithFigTableBB(caption:Caption, figTableBB:Region)

case class Figure(figTableBB:Region,words:Seq[PdfWord])

case class FigureCaption(caption:Caption,figure:Figure)



/******************************************************************************************************/
case class point (x:Float,y:Float)
case class VectorGraphicsPathString(pathContent:String,boundingBox:Rectangle, pageNumber:Int)
//case class VectorGraphicsImage(paths:Seq[VectorGraphicsPathString], pageNumber:Int)

/******************************************************************************************************/
/**
 * String of SVG paths with caption.
 */
case class CaptionWithFigTableSVG(caption:Caption, figTableBB:Region, svgPaths:Seq[VectorGraphicsPathString])

/******************************************************************************************************/
/**
 * A representation for a PDF page. Contains all objects that appear on a single page,
 * including the graphic lines, rectangles, images, individual characters, words,
 * text lines, and paragraphs.
 */
case class PdPageObject(
  pageBBs:      Seq[Rectangle],
  pdLines:      Seq[PdfLine],
  pdRects:      Seq[PdfRect],
  pdImages:     Seq[PdfRaster],
  pdChars:      Seq[PdfChar],
  pdWords:      Seq[PdfWord],
  pdTextLines:  Seq[PdfTextLine],
  pdParagraphs: Seq[PdfParagraph]
)

object PdPageObject {

  /**
   * A PdPageObject whose fields are all empty sequences.
   */
  val empty: PdPageObject =
    PdPageObject(
      Seq.empty[Rectangle],
      Seq.empty[PdfLine],
      Seq.empty[PdfRect],
      Seq.empty[PdfRaster],
      Seq.empty[PdfChar],
      Seq.empty[PdfWord],
      Seq.empty[PdfTextLine],
      Seq.empty[PdfParagraph]
    )
}