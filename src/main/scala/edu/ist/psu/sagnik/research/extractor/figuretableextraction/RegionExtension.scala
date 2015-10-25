package edu.ist.psu.sagnik.research.extractor.figuretableextraction

import edu.ist.psu.sagnik.research.extractor.FigureTableExtraction.TextBlockClassification._
import edu.ist.psu.sagnik.research.extractor.FigureTableExtraction.TextBlockClassificationHelper._
import edu.ist.psu.sagnik.research.extractor.captionmentionextraction.CaptionMention
import edu.ist.psu.sagnik.research.extractor.model.Rectangle._
import edu.ist.psu.sagnik.research.extractor.model._

/**
 * Created by szr163 on 10/16/15.
 */
object RegionExtension {

  def apply(extractedPdf: Seq[PdPageObject]):Seq[CaptionAdjRegion]={
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
    val classifiedTBs= classifyTextBlocks(tbWithColNumbers,captionBBs,colMarginXs,pdGraphics,pdImages,pageBB)

    //TODO: check if this removes spurious graphics paths such as symbols in equations and stuff like that


    val validGraphics= pdGraphics.filterNot(a=>
      classifiedTBs.bodyTbs.exists(p=> p.pageNumber==a.pageNumber &&
        rectInterSects(rectAllSideExtension(p.boundingBox,-2,pageBB),rectAllSideExtension(a.boundingBox,-2,pageBB)))
    )

    val validRaster= pdImages.filterNot(a=>
      a.boundingBox.y2-a.boundingBox.y1<5||
        a.boundingBox.x2-a.boundingBox.x1<5 ||
        classifiedTBs.bodyTbs.exists(p=> p.pageNumber==a.pageNumber &&
          rectInterSects(rectAllSideExtension(p.boundingBox,-2,pageBB),rectAllSideExtension(a.boundingBox,-2,pageBB)))
    )

    //validRaster.foreach(println)


//    println(s"[graphics regions before/after filtering] ${pdGraphics.length},${validGraphics.length} " +
//      s"[image regions before/after filtering] ${pdImages.length}, ${validRaster.length}")

    val captionsWithAdjRegions=captions.map(p=>CaptionAdjRegion(p,getAdjacentRegions(p,pageBB,classifiedTBs,validGraphics,validRaster,numberOfColumns)))
    //captionWithRegionsPrettyPrint(captionsWithAdjRegions)
    captionsWithAdjRegions

  }


  def getAdjacentRegions(p:Caption, pageBB:Rectangle, classifiedTBs:ClassifiedTextBlocks, validGraphics:Seq[PdfLine],
                         validRaster:Seq[PdfRaster], numberOfColumns:Int):AdjRegions={

    val btbsThisPage=classifiedTBs.bodyTbs.filter(a=>a.pageNumber==p.pageNumber)
    val ctbsThisPage=classifiedTBs.captionTbs.filter(a=>a.pageNumber==p.pageNumber)
    val figTabletbsThisPage=classifiedTBs.figTableTbs.filter(a=>a.pageNumber==p.pageNumber)
    val validGraphicsThisPage=validGraphics.filter(a=>a.pageNumber==p.pageNumber)
    val validRasterThisPage=validRaster.filter(a=>a.pageNumber==p.pageNumber)

      /*
    TODO: Important assumption: CaptionBBs can only be extended hoprizontally and when extended, they will either
    span the column or the whole page. This needs to be changed later.
     */


    val extendedCaptionBB= numberOfColumns match {
      case 1 => Rectangle(pageBB.x1+1,p.boundingBox.y1,pageBB.x2-1,p.boundingBox.y2)
      case 2 => if (p.colNo==1) {
        if (p.spansMultiColumn) Rectangle(pageBB.x1+1,p.boundingBox.y1,pageBB.x2-1,p.boundingBox.y2)
        else Rectangle(pageBB.x1+1,p.boundingBox.y1,scala.math.round((pageBB.x1+pageBB.x2)/2),p.boundingBox.y2)
      }
      else Rectangle(scala.math.round((pageBB.x1+pageBB.x2)/2),p.boundingBox.y1,pageBB.x2-1,p.boundingBox.y2)
      case _ => Rectangle(pageBB.x1+1,p.boundingBox.y1,pageBB.x2-1,p.boundingBox.y2)
      // TODO: we should really do more involved stuff here, afterall the document has 3 columns. but yeah, who gives. You creator of three column document, die in hell.
    }

    /*
    println(s"[caption content]: ${p.content} [col]: ${p.colNo} [isSpanning]: ${p.spansMultiColumn}" +
      s" [caption bb before]: ${asCoordinatesStr(p.boundingBox)} [caption bb after expansion]: ${asCoordinatesStr(extendedCaptionBB)}")
    */

    val (left, right, top, down)=(
      rectExtend(Rectangle(extendedCaptionBB.x1-2,extendedCaptionBB.y1,extendedCaptionBB.x1-1,extendedCaptionBB.y2),
        btbsThisPage.map(a=>a.boundingBox)++ctbsThisPage.map(a=>a.boundingBox),
        figTabletbsThisPage.map(a=>a.boundingBox)++validGraphicsThisPage.map(a=>a.boundingBox)++validRasterThisPage.map(a=>a.boundingBox),
        pageBB,"left") match{ case Some(bb) => Some(Region(bb,p.pageNumber)) case _ => None},

      rectExtend(Rectangle(extendedCaptionBB.x2+1,extendedCaptionBB.y1,extendedCaptionBB.x2+2,extendedCaptionBB.y2),
        btbsThisPage.map(a=>a.boundingBox)++ctbsThisPage.map(a=>a.boundingBox),
        figTabletbsThisPage.map(a=>a.boundingBox)++validGraphicsThisPage.map(a=>a.boundingBox)++validRasterThisPage.map(a=>a.boundingBox),
        pageBB,"right") match{ case Some(bb) => Some(Region(bb,p.pageNumber)) case _ => None},

      rectExtend(Rectangle(extendedCaptionBB.x1,extendedCaptionBB.y2+1,extendedCaptionBB.x2,extendedCaptionBB.y2+2),
        btbsThisPage.map(a=>a.boundingBox)++ctbsThisPage.map(a=>a.boundingBox),
        figTabletbsThisPage.map(a=>a.boundingBox)++validGraphicsThisPage.map(a=>a.boundingBox)++validRasterThisPage.map(a=>a.boundingBox),
        pageBB,"top") match{ case Some(bb) => Some(Region(bb,p.pageNumber)) case _ => None},

      rectExtend(Rectangle(extendedCaptionBB.x1,extendedCaptionBB.y1-2,extendedCaptionBB.x2,extendedCaptionBB.y1-1),
        btbsThisPage.map(a=>a.boundingBox)++ctbsThisPage.map(a=>a.boundingBox),
        figTabletbsThisPage.map(a=>a.boundingBox)++validGraphicsThisPage.map(a=>a.boundingBox)++validRasterThisPage.map(a=>a.boundingBox),
        pageBB,"down") match{ case Some(bb) => Some(Region(bb,p.pageNumber)) case _ => None}
      )
    //TODO: a bit of code restructuring here. The regions should be filtered finally, not in the beginning.

    if (List(left,right,top,down).flatten.length==0) // we have None for all directions, take a random guess now, which will be most likely a rectangle on top of the caption
      AdjRegions(left,down,right,top=Some(Region(Rectangle(extendedCaptionBB.x1,extendedCaptionBB.y2+1,extendedCaptionBB.x2,extendedCaptionBB.y2+200),p.pageNumber)))
    else

    AdjRegions(
      left,down,right,top
    )
  }


  def captionWithRegionsPrettyPrint(cs:Seq[CaptionAdjRegion]):Unit=
    cs.foreach(a=>println(
      s"[content]: ${a.caption.content} \n" +
        s"[colNo]: ${a.caption.colNo} [Spans Multi Column]: ${a.caption.spansMultiColumn} \n"+
        s"[bb]: ${asCoordinatesStr(a.caption.boundingBox)} \n" +
        s"[leftbb]: ${a.adjRegions.left match { case Some(r) => asCoordinatesStr(r.bb); case _ => "None"} } \n"+
        s"[downbb]: ${a.adjRegions.down match { case Some(r) => asCoordinatesStr(r.bb); case _ => "None"} } \n"+
        s"[rightbb]: ${a.adjRegions.right match { case Some(r) => asCoordinatesStr(r.bb); case _ => "None"} } \n"+
        s"[topbb]: ${a.adjRegions.top match { case Some(r) => asCoordinatesStr(r.bb); case _ => "None"} } \n"
    )
    )

}
