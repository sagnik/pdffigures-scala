package edu.ist.psu.sagnik.research.extractor.FigureTableExtraction

import edu.ist.psu.sagnik.research.extractor.captionmentionextraction.CaptionMention
import edu.ist.psu.sagnik.research.extractor.model._
import edu.ist.psu.sagnik.research.extractor.model.Rectangle._

import edu.ist.psu.sagnik.research.extractor.FigureTableExtraction.TextBlockClassificationHelper._
/**
 * Created by szr163 on 10/8/15.
 */
object TextBlockClassification {


  val MARGINTOLERANCE=2f

  def apply(extractedPdf: Seq[PdPageObject]):ClassifiedTextBlocks={

      val pageBB= extractedPdf.flatMap(a=>a.pageBBs).headOption match{
      case Some(pageBB)=>pageBB
      case _ => {Rectangle(0f,0f,595f,842f)} //standard US A4 letter size in points, https://www.gnu.org/software/gv/manual/html_node/Paper-Keywords-and-paper-size-in-points.html
    }

    val pdTextLines= extractedPdf.flatMap(p=>p.pdTextLines)
    val pdPargaraphs= extractedPdf.flatMap(p=>p.pdParagraphs)
    val pdGraphics = extractedPdf.flatMap(p=>p.pdLines)
    val pdImages= extractedPdf.flatMap(p=>p.pdImages)

    val tbs=getTbs(pdPargaraphs,pdTextLines)

    val bb=pageBB
    val numberOfColumns= {
      if (tbs.filter(x => rectInterSects(x.boundingBox, Rectangle((bb.x1 + bb.x2) / 3, bb.y1, ((bb.x1 + bb.x2) / 3) +1, bb.y2))).length<0.1*tbs.length) 3
      else if (tbs.filter(x => rectInterSects(x.boundingBox, Rectangle((bb.x1 + bb.x2) / 2, bb.y1, ((bb.x1 + bb.x2) / 2) +1, bb.y2))).length<0.1*tbs.length) 2
      else 1
    }

    val tbWithColNumbers= getColNumber(tbs,pageBB,numberOfColumns)
    val colMarginXs= if (getColMarginXs(tbWithColNumbers).length>0)
      getColMarginXs(tbWithColNumbers)
    else List(20f).toIndexedSeq

    val captions=CaptionMention.Load(pdTextLines,pdPargaraphs,pdGraphics,pdImages,colMarginXs)
    val captionBBs=captions.map(p=>(p.boundingBox,p.pageNumber))

    colMarginXs.zipWithIndex.foreach{case(a,b)=>println(s"[coulmn number]: ${b+1} , [x1 value]: $a")}
    captions.foreach(a=>println(s"[caption]: ${a.content} [bounding box]: ${a.boundingBox} [column number]: ${a.colNo}, [spans multi column]: ${a.spansMultiColumn}"))
    val classifiedTextBlocks=classifyTextBlocks(tbWithColNumbers,captionBBs,colMarginXs,pdGraphics,pdImages,pageBB)
    //printClassificationResults(classifiedTextBlocks)
    classifiedTextBlocks
  }


  def classifyTextBlocks(tbs:Seq[D],captionBBs:Seq[(Rectangle,Int)],colMarginXs:Seq[Float],pdGraphics:Seq[PdfLine],pdImages:Seq[PdfRaster],pageBB:Rectangle):ClassifiedTextBlocks={
    //caption identification
    val ctbs=tbs.foldLeft(Seq.empty[A])(
      (accum,tb)=>
        if (captionBBs.exists(p=>rectInterSects(p._1,tb.boundingBox) && p._2==tb.pageNumber) &&
          tb.content.replaceAll("\\P{Alnum}","").length>0)
          accum:+A(tb.boundingBox,tb.pageNumber,tb.content,tb.columnNo)
        else accum
    )

    /*
  We will check if there exists a page spanning line within top 10% of the page or any line within the below 10% of the page. If such a line exists,
  we will classify the text blocks above the top line and below the bottom line as a body text block. They are actually header/footers but for our
  purpose this suffices.
  */
    val hfLines=getHFLines(pdGraphics,pageBB,colMarginXs)
    hfLines.foreach(a=>println(s"[header/footer found] [page number]: ${a.pageNumber} [bounding box]: ${asCoordinatesStr(a.boundingBox)} [isheader]: ${a.isTop}"))


    /* Body block identification: classifying text blocks as body text is a two-pass algorithm, first pass: get text blocks that do not intersect with caption blocks, the second pass:
     * get text blocks that are vertically very close to text blocks already classified as body text (we assume that they are in the same line)
     */
    val tempbtbs=tbs.foldLeft(Seq.empty[B])(
      (accum,tb)=>
        if (
          (
            (!ctbs.exists(p=>rectInterSects(p.boundingBox,tb.boundingBox) && p.pageNumber==tb.pageNumber) && //doesn't intersect with a caption
              colMarginXs.exists(p=>scala.math.abs(p-tb.boundingBox.x1)<MARGINTOLERANCE)) //almost aligned with the margins
              ||
              hfLines.exists(p=>(tb.pageNumber==p.pageNumber && p.isTop && tb.boundingBox.y1>p.boundingBox.y2)|| //above or below header/footer
                (tb.pageNumber==p.pageNumber&& !p.isTop && tb.boundingBox.y2<p.boundingBox.y1))
            )
            &&
            tb.content.replaceAll("\\P{Alnum}","").length>0
        )  accum:+B(tb.boundingBox,tb.pageNumber,tb.content,tb.columnNo)
        else accum
    )

    val btbs=tempbtbs ++
      tbs.foldLeft(Seq.empty[B])(
        (accum,tb)=>
          if (tempbtbs.exists(p=>
            p.pageNumber==tb.pageNumber &&
              scala.math.abs(p.boundingBox.y1-tb.boundingBox.y1)<1f &&
              scala.math.abs(p.boundingBox.y2-tb.boundingBox.y2)<1f &&
              (scala.math.abs(tb.boundingBox.x1-p.boundingBox.x2)< 20f || //TODO:check this
              scala.math.abs(tb.boundingBox.x2-p.boundingBox.x1)< 20f)
          ) &&
            tb.content.replaceAll("\\P{Alnum}","").length>0
          )
            accum:+B(tb.boundingBox,tb.pageNumber,tb.content,tb.columnNo)

          else accum
      )


    // table figure text identification: Remember this will possibly classify stuff like paper title and lists as figure/ text blocks.

    val tableFigTbs=tbs.foldLeft(Seq.empty[C])(
      (accum,tb)=> //doesn't intersect with a caption, or a body text
        if (!(
          ctbs.exists(p=>(p.pageNumber==tb.pageNumber) &&
            rectInterSects(p.boundingBox,tb.boundingBox))
          ) &&
          !(
            btbs.exists(p=>(p.pageNumber==tb.pageNumber) &&
              rectInterSects(p.boundingBox,tb.boundingBox))
            ) &&
          tb.content.replaceAll("\\P{Alnum}","").length>0
        )
          accum:+C(tb.boundingBox,tb.pageNumber,tb.content,tb.columnNo)
        else accum
    )

    //heuristic for removing section headers: this text block should hit multiple body text blocks when
    // extended

    ClassifiedTextBlocks(ctbs,
      btbs++tableFigTbs.filter(p => isSectionHeader(p,btbs,ctbs,pageBB,colMarginXs.length)).map(a=>B(a.boundingBox,a.pageNumber,a.content,a.columnNo)),
      tableFigTbs.filterNot(p => isSectionHeader(p,btbs,ctbs,pageBB,colMarginXs.length))
    )
  }


}
