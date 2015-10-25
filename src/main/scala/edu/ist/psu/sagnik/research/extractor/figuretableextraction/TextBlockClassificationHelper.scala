package edu.ist.psu.sagnik.research.extractor.FigureTableExtraction

import edu.ist.psu.sagnik.research.extractor.model.Rectangle._
import edu.ist.psu.sagnik.research.extractor.model._

/**
 * Created by szr163 on 10/14/15.
 */
object TextBlockClassificationHelper {
  type A=CaptionTextBlock
  def A(r:Rectangle,p:Int,c:String,cn:Int)=CaptionTextBlock(r,p,c,cn)
  type B=BodyTextBlock
  def B(r:Rectangle,p:Int,c:String,cn:Int)=BodyTextBlock(r,p,c,cn)
  type C=FigTableTextBlock
  def C(r:Rectangle,p:Int,c:String,cn:Int)=FigTableTextBlock(r,p,c,cn)
  type D=PdfTextBlock
  def D(r:Rectangle,p:Int,c:String,cn:Int)=PdfTextBlock(r,p,c,1)

  /*
  * TODO: Explain the problem with PdfXTk
  * */
  def getTbs(pdPargaraphs: Seq[PdfParagraph], pdTextLines: Seq[PdfTextLine])=
    pdPargaraphs.map(a=>D(a.boundingBox,a.pageNumber,a.content,1)) ++
      pdTextLines.foldLeft(Seq.empty[D])(
        (accum,tl)=>
          if (!pdPargaraphs.exists(p=>p.pageNumber==tl.pageNumber && rectInterSects(p.boundingBox,tl.boundingBox)))
            accum:+D(tl.boundingBox,tl.pageNumber,tl.content,1)
          else accum
      )


  def getColNumber(tbs:Seq[D],bb:Rectangle,columnNumber:Int):Seq[D]={
    columnNumber match{
      case 1 => tbs
      case 2 => tbs.map(x=> if (x.boundingBox.x1<(bb.x1 + bb.x2) / 2) x else x.copy(columnNo = 2))
      case 3 => tbs.map(x=> if (x.boundingBox.x1<(bb.x1 + bb.x2) / 3) x else if (x.boundingBox.x1<(bb.x1 + bb.x2)*0.67) x.copy(columnNo = 2) else x.copy(columnNo = 3))
    }
  }


  def getColMarginXs(tbs:Seq[D]):Seq[Float]={
    tbs.groupBy(a=>a.columnNo).values
      .map(a=>a.groupBy(b=>b.boundingBox.x1)
      .toIndexedSeq
      .sortWith(_._2.length>_._2.length)(0)._1 //TODO: Only considering the frequency of text blocks here, should we consider number of chars in them?
      ).toIndexedSeq.sorted
  }

  /*
  We first extend a possible figure/table text block vertically by 300 points and 300 points downwards and see if it intersects any
  (expanded) caption block. If not, it is a section header text with high probability. If it does, we check what is the nearest caption block
    and see if there's a body text block between this figure/table text block and the nearest caption block. If yes, we
    think it is a section header, else we think it is a figure/table text.
   */

  def isSectionHeader(p:C,btbs:Seq[B], ctbs:Seq[A],pageBB:Rectangle, noCols:Int):Boolean= {
    val tbb=p.boundingBox
    if (!ctbs.exists(a => a.pageNumber==p.pageNumber &&
      rectInterSects(
        rectBothYExtension(tbb, 300, pageBB), //TODO: This extension by 5 is arbitrary, might fail in captions spanning whole page in a double column document. See rectColAwareHorizontalExtension method in Rectangle.scala
        colAwareCaptionBBXExtension(a,pageBB,noCols)
      )
    )
    ) true //doesn't intersect with any caption
    else {
      //false
      val c = ctbs.filter(a => a.pageNumber==p.pageNumber &&
        rectInterSects(rectBothYExtension(tbb, 300, pageBB),
          colAwareCaptionBBXExtension(a,pageBB,noCols)
        )
      ).sortWith((a,b)=>rectDistance(tbb,a.boundingBox)<rectDistance(tbb,b.boundingBox)) //get captions with which the extended rect intersects
      //sorted by the vertical distance from this text block, increasing order

           btbs.exists(a => a.pageNumber==p.pageNumber && rectInterSects(a.boundingBox, rectMerge(tbb, c(0).boundingBox))) //make sure there's no text block that is inside this region.
    }
  }


  /*
  * Method to get header/ footer lines. Remember that the co ordinate system is at the bottom left corner of the page. Y increases upwards.
  * */
  def getHFLines(pdGraphics:Seq[PdfLine],pageBB:Rectangle,colMarginXs:Seq[Float]):Seq[HeaderFooterLine]=
    pdGraphics.foldLeft(Seq.empty[HeaderFooterLine])(
      (accum,line)=>(
        if (line.boundingBox.y1>pageBB.y2*0.8 && (line.boundingBox.x2-line.boundingBox.x1)>40f &&
          !pdGraphics.exists(a=>a.boundingBox.y1>line.boundingBox.y2 && a.pageNumber==line.pageNumber)
        )
          accum:+HeaderFooterLine(line.boundingBox,line.pageNumber,true)
        else if (
          (line.boundingBox.y2<pageBB.y2*0.2) && (line.boundingBox.x2-line.boundingBox.x1)>20f &&
            !pdGraphics.exists(a=>a.boundingBox.y2<line.boundingBox.y1 && a.pageNumber==line.pageNumber)
        )
          accum:+HeaderFooterLine(line.boundingBox,line.pageNumber,false)
        else accum
        )
    )

  def colAwareCaptionBBXExtension(p:A, pageBB:Rectangle, noCols:Int):Rectangle=
    noCols match {
      case 1 => Rectangle(pageBB.x1+1,p.boundingBox.y1,pageBB.x2-1,p.boundingBox.y2)
      case 2 => if (p.columnNo==1) {
        if (p.boundingBox.x2>(pageBB.x1+pageBB.x2)/2) Rectangle(pageBB.x1+1,p.boundingBox.y1,pageBB.x2-1,p.boundingBox.y2) //implies caption spans column.
        else Rectangle(pageBB.x1+1,p.boundingBox.y1,scala.math.round((pageBB.x1+pageBB.x2)/2),p.boundingBox.y2)
      }
      else Rectangle(scala.math.round((pageBB.x1+pageBB.x2)/2),p.boundingBox.y1,pageBB.x2-1,p.boundingBox.y2)
      case _ => Rectangle(pageBB.x1+1,p.boundingBox.y1,pageBB.x2-1,p.boundingBox.y2)
      // TODO: we should really do more involved stuff here, afterall the document has 3 columns. but yeah, who gives. You creator of three column document, die in hell.
    }



  def printClassificationResults(allTextBlocks:ClassifiedTextBlocks):Unit={
    val pn=13
    println("\n--------------------------------\nBody Text Blocks\n--------------------------------\n")
    allTextBlocks.bodyTbs.foreach(a=>if (a.pageNumber==pn) println(s"[type]: Body [content]: ${a.content} [page number]: ${a.pageNumber} " +
      s"[column number]: ${a.columnNo} [bounding box]: ${asCoordinatesStr(a.boundingBox)}"))


    println("\n--------------------------------\nCaption Text Blocks\n--------------------------------\n")
    allTextBlocks.captionTbs.foreach(a=>if (a.pageNumber==pn) println(s"[type]: Caption [content]: ${a.content} [page number]: ${a.pageNumber} " +
      s"[column number]: ${a.columnNo} [bounding box]: ${asCoordinatesStr(a.boundingBox)}"))


    println("\n--------------------------------\nFigure/Table Text Blocks\n--------------------------------\n")
    allTextBlocks.figTableTbs.foreach(a=>if (a.pageNumber==pn) println(s"[type]: Figure/Table [content]: ${a.content} [page number]: ${a.pageNumber} " +
      s"[column number]: ${a.columnNo} [bounding box]: ${asCoordinatesStr(a.boundingBox)}"))

  }

}
