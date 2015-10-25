package edu.ist.psu.sagnik.research.extractor.captionmentionextraction

import edu.ist.psu.sagnik.research.extractor.model._
import edu.ist.psu.sagnik.research.extractor.regex.RegexTestHarness
import edu.ist.psu.sagnik.research.extractor.model.Rectangle._

/**
 * Created by szr163 on 10/7/15.
 */
object CaptionMentionExtractionHelper {

  type captions = Seq[Caption]
  type mentions = Seq[Mention]
  type A = PdfTextLine
  def A(r:Rectangle,p:Int,c:String)= PdfTextLine(r,p,c)
  type B = PdfParagraph
  type C = PdfLine
  type D = PdfRaster

  val TOLERANCE = 5.0f
  val LINEMERGETHRESHOLD=3.0f //TODO: this is heuristic so far

  def convertToCaption(pdLine: A, pdParagraphs: Seq[B], pdGraphics: Seq[C], pdImages: Seq[D], colMarginXs: Seq[Float]): Option[Caption] = {

    val objectTextStartIndex = math.max(0, new RegexTestHarness().indexAfterCaptionID(pdLine.content))
    if (objectTextStartIndex == 0 || objectTextStartIndex == pdLine.content.length) return None
    val text = pdLine.content.trim().toLowerCase
    val (objectId, captionType) =
      if (text.startsWith("fig")) {
        if (text.startsWith("figure")) (pdLine.content.substring(6, objectTextStartIndex).replaceAll("\\P{Alnum}", ""), "Figure")
        else (pdLine.content.substring(4, objectTextStartIndex).replaceAll("\\P{Alnum}", ""), "Figure") //replacing everything in figureid that is not alpha neumeric
      }
      else {
        (pdLine.content.substring(5, objectTextStartIndex).replaceAll("\\P{Alnum}", ""), "Table") //table caption
      }
    val captionParagraph = pdParagraphs.filter(p =>p.pageNumber==pdLine.pageNumber && rectInterSects(p.boundingBox,pdLine.boundingBox)) //this should return only 0 or 1 element

    if (captionParagraph.length > 0) {
      //println(s"content ${captionParagraph(0).content}, confidence: ${getConfidence(captionParagraph(0).content, captionParagraph(0).boundingBox, objectTextStartIndex, pdGraphics, pdImages)}")
      Some(Caption(captionParagraph(0).boundingBox,
        captionParagraph(0).pageNumber,
        captionParagraph(0).content,
        objectId,
        captionType,
        captionType + objectId,
        getConfidence(captionParagraph(0).content, captionParagraph(0).boundingBox, objectTextStartIndex, pdGraphics, pdImages),
        false,
      getColNo(captionParagraph(0).boundingBox,colMarginXs),
      isSpanning(captionParagraph(0).boundingBox,colMarginXs))
      )
    }
    else {
      //println(s"content ${pdLine.content}|| confidence: ${getConfidence(pdLine.content, pdLine.boundingBox, objectTextStartIndex, pdGraphics, pdImages)}")
      Some(Caption(pdLine.boundingBox,
        pdLine.pageNumber,
        pdLine.content,
        objectId,
        captionType,
        captionType + objectId,
        getConfidence(pdLine.content, pdLine.boundingBox, objectTextStartIndex, pdGraphics, pdImages),
        true,
        getColNo(pdLine.boundingBox,colMarginXs),
        isSpanning(pdLine.boundingBox,colMarginXs))
      )
    }
  }

  def checkForParagraphs(caption: Caption, pdLines: Seq[A]): Caption = {
    if (!caption.isTextLine) caption
    else {
      //check if we can increase the boundary to include other lines

      val candidateTextLines = A(caption.boundingBox, caption.pageNumber, caption.content) +:
        pdLines
          .filter(a => {
          val thisbb = a.boundingBox
          val captionbb = caption.boundingBox
          a.boundingBox != caption.boundingBox && //don't take the line itself
            a.pageNumber == caption.pageNumber &&
            thisbb.y1 < captionbb.y2 && //Y increases upwards from lower left corner
            scala.math.abs(thisbb.x1 - captionbb.x1) < TOLERANCE &&
            thisbb.x2 <= captionbb.x2 + 3f
        })
          .sortWith(_.boundingBox.y1 > _.boundingBox.y1)

      //println(s"${caption.content}, ${candidateTextLines.length}")
      if (candidateTextLines.length <= 1)
        convertLineSeqtoCaptionParagraph(candidateTextLines, caption)

      else if (candidateTextLines.length == 2) {
        if (candidateTextLines(0).boundingBox.y1 - candidateTextLines(1).boundingBox.y2 < LINEMERGETHRESHOLD)
          convertLineSeqtoCaptionParagraph(candidateTextLines, caption)
        else
          convertLineSeqtoCaptionParagraph(Seq.empty[A] :+ candidateTextLines(0), caption)
      }
      else {
        val captionLinesIndices = (0 to candidateTextLines.length - 2)
          .iterator
          .takeWhile(a => candidateTextLines(a).boundingBox.y1 - candidateTextLines(a + 1).boundingBox.y2 < LINEMERGETHRESHOLD)
          .toList


        if (captionLinesIndices.isEmpty) {
          if (candidateTextLines(0).boundingBox.y1 - candidateTextLines(1).boundingBox.y2 < LINEMERGETHRESHOLD)
            convertLineSeqtoCaptionParagraph(Seq.empty[A] :+ candidateTextLines(0) :+ candidateTextLines(1), caption)
          else
            convertLineSeqtoCaptionParagraph(Seq.empty[A] :+ candidateTextLines(0), caption)
        }

        else {
          //println(s"last= ${captionLinesIndices.last}, ${candidateTextLines(captionLinesIndices.last)} -> ${candidateTextLines(captionLinesIndices.last+1)}")
          if (candidateTextLines(captionLinesIndices.last).boundingBox.y1 - candidateTextLines(captionLinesIndices.last + 1).boundingBox.y2 < LINEMERGETHRESHOLD)
            convertLineSeqtoCaptionParagraph(candidateTextLines.slice(0, captionLinesIndices.last + 2), caption)
          else
            convertLineSeqtoCaptionParagraph(candidateTextLines.slice(0, captionLinesIndices.last+1), caption)
        }
        //println(s"original caption line: ${caption}")
        //candidateTextLines.foreach(a => println(s"[inferred caption line]: ${a}\n\n"))
        //captionLines.foreach(a => println(s"[final caption line]: ${a}\n\n"))
      }
    }

  }




  def convertLineSeqtoCaptionParagraph(lines:Seq[A],caption:Caption):Caption={
    if (lines.length==1)
      caption
    else {
      caption.copy(
        boundingBox=new Rectangle(
          lines.map(a=>a.boundingBox.x1).reduceLeft(_ min _),
          lines.map(a=>a.boundingBox.y1).reduceLeft(_ min _),
          lines.map(a=>a.boundingBox.x2).reduceLeft(_ max _),
          lines.map(a=>a.boundingBox.y2).reduceLeft(_ max _)
        ),
        content = lines.foldLeft("")((r,c)=>r+c.content),
        isTextLine = false
      )
    }
  }

  def getConfidence(text: String, bb:Rectangle, start: Int, pdGraphics:Seq[C], pdImages: Seq[D]): Float = {
    //println(s"confidence ${text}")

    if ((pdGraphics.filter(a=>rectInterSects(Rectangle(bb.x1-50f,bb.y1-50f,bb.x2+50f,bb.y2+50f),a.boundingBox)).length+
      pdImages.filter(a=>rectInterSects(Rectangle(bb.x1-50f,bb.y1-50f,bb.x2+50f,bb.y2+50f),a.boundingBox)).length)<1)
      return -1
    if (text.substring(start).toLowerCase().startsWith("show"))
      return -1
    val puncConfidence=if (text.substring(0, start).contains(":")) 2f else if (text.substring(0, start).contains(".")) 1f else 0f
    implicit def boolean2Int(b:Boolean):Float= if (b) 1f else 0f
    text.charAt(start).isUpper +
      puncConfidence+
      0.01f*
      (pdGraphics.filter(a=>rectInterSects(Rectangle(bb.x1-50f,bb.y1-50f,bb.x2+50f,bb.y2+50f),a.boundingBox)).length+
        pdImages.filter(a=>rectInterSects(Rectangle(bb.x1-50f,bb.y1-50f,bb.x2+50f,bb.y2+50f),a.boundingBox)).length)
  }

  /*
  def getMentionParagraphs(paragraphs:Seq[B]): mentions = paragraphs.filter(p=>p.content.contains("Table")
    ||p.content.startsWith("Table"))
    */

  def getColNo(bb:Rectangle,colMarginX:Seq[Float]):Int={
    if (colMarginX.length==3) {
      if (bb.x2 < colMarginX(1)) 1
      else if ((bb.x1>= colMarginX(1) && bb.x2< colMarginX(2))) 2
      else if (bb.x1>= colMarginX(2)) 3
      else 1
    }
    else if (colMarginX.length==2) {
      if (bb.x2 < colMarginX(1)) 1
      else if (bb.x1>= colMarginX(1)) 2
      else 1
    }
    else 1

  }

  def isSpanning(bb:Rectangle,colMarginX:Seq[Float]):Boolean={
    if (colMarginX.length==3) rectInterSects(bb,Rectangle(colMarginX(1)-3,0,colMarginX(1)-2,1000)) || rectInterSects(bb,Rectangle(colMarginX(2)-3,0,colMarginX(2)-2,1000))
    else if (colMarginX.length==2) rectInterSects(bb,Rectangle(colMarginX(1)-3,0,colMarginX(1)-2,1000))
    else false
  }
}
