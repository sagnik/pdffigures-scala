package edu.ist.psu.sagnik.research.extractor.figuretableextraction

import java.io.File

import edu.ist.psu.sagnik.research.extractor.FigureTableExtraction.TextBlockClassification._
import edu.ist.psu.sagnik.research.extractor.FigureTableExtraction.TextBlockClassificationHelper._
import edu.ist.psu.sagnik.research.extractor.captionmentionextraction.CaptionMention
import edu.ist.psu.sagnik.research.extractor.model.Rectangle._
import edu.ist.psu.sagnik.research.extractor.model._
import edu.ist.psu.sagnik.research.extractor.writers.{HTMLWriter, SVGWriter}

import org.xmlcml.pdf2svg.PDF2SVGConverter

import scala.xml.Node


/**
 * Created by szr163 on 10/22/15.
 */
object FigureTableExtractionSVG {

  val badRectangle=Rectangle(-30,-30,-30,-30)

  def apply(extractedPdf: Seq[PdPageObject], pdfloc: String):Unit={

    /*IMPORTANT: This mapping is done because the SVG produced by pdf2svg has the opposite coordinate system of pdfXtk. In pdf2Xtk bottom left corner is the origin,
    * in pdf2svg top left corner is the origin*/

    val pageBB= extractedPdf.flatMap(a=>a.pageBBs).headOption match{
      case Some(pageBB)=>pageBB
      case _ => {Rectangle(0f,0f,595f,842f)} //standard US A4 letter size in points, https://www.gnu.org/software/gv/manual/html_node/Paper-Keywords-and-paper-size-in-points.html
    }

    //TODO: need to do a bit of code restructuring her, the portion between /** and **/ is repeated code
    /*************************************************************************************************/
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

    /********************************************************************************************/
    val figTableCaptionBBs=RegionRanking(extractedPdf).

      map(x=>CaptionWithFigTableBB(
      x.caption,
      x.figTableBB.copy(bb = filterRegion(x.figTableBB.bb,
        classifiedTBs.figTableTbs.filter(y=>y.pageNumber==x.caption.pageNumber),
        validGraphics.filter(y=>y.pageNumber==x.caption.pageNumber),
        validRaster.filter(y=>y.pageNumber==x.caption.pageNumber),
        pageBB
      )
      )
    )
      ).
      map(
        x=>CaptionWithFigTableBB(
          x.caption.copy(boundingBox = Rectangle(x.caption.boundingBox.x1,pageBB.y2-x.caption.boundingBox.y2,x.caption.boundingBox.x2,pageBB.y2-x.caption.boundingBox.y1)),
          x.figTableBB.copy(bb = Rectangle(x.figTableBB.bb.x1,pageBB.y2-x.figTableBB.bb.y2,x.figTableBB.bb.x2,pageBB.y2-x.figTableBB.bb.y1))
        )
      )
    figTableCaptionBBs.foreach(x=>println(s"[caption content]: ${x.caption.content} [caption bb]: ${x.caption.boundingBox} [figure bb]: ${x.figTableBB.bb}"))


    if (!new File(pdfloc.substring(0,pdfloc.length-4)).exists() || ! new File(pdfloc.substring(0,pdfloc.length-4)).isDirectory)
      new File(pdfloc.substring(0, pdfloc.length - 4)).mkdir()
    else
      println(s"target directory ${pdfloc.substring(0, pdfloc.length - 4)} already exists!")


    if (new File(pdfloc.substring(0,pdfloc.length-4)).exists() && new File(pdfloc.substring(0,pdfloc.length-4)).isDirectory) {
      val SVGConversionSucceded = new PDF2SVGConverter().run(
        "-logger",
        "-infofiles",
        "-logglyphs",
        "-outdir", pdfloc.substring(0,pdfloc.length-4),
        pdfloc
      )
    }

    /*TODO: This is taking a long time, need a faster way*/
    val svgFiles=DataLocation(new File(pdfloc.substring(0,pdfloc.length-4)),"page\\d+.svg"r).map(x=>x.getAbsolutePath)
    val pathsAllPage= svgFiles
      .flatMap(
        a=>{
          io.Source.fromFile(a).mkString.split("\n").drop(5).dropRight(1).map(x=>x.trim).filter(x=>x.startsWith("<path") || x.startsWith("<text") || x.startsWith("<image")).toIndexedSeq
            .map(
              x=> VectorGraphicsPathString(x,
                getPathBoundingBox(x),
                a.substring(0,a.length-4).split("-").last.replace("page","").toInt //TODO: possible exception, should be handled
              )
            )
        }
      ).toIndexedSeq

    //pathsAllPage.foreach(a=> if (a.pageNumber==12 && a.pathContent.startsWith("<image")) println(a.pathContent))
    val svgFiguresCaptions=figTableCaptionBBs.map(x=>
      CaptionWithFigTableSVG(x.caption,x.figTableBB,
        pathsAllPage.filter(y=>y.pageNumber==x.caption.pageNumber && rectInside(y.boundingBox,x.figTableBB.bb,2f))
      )
    )

    svgFiguresCaptions.foreach(x=>SVGWriter(
      x,
      pdfloc.substring(0,pdfloc.length-4)+"/"+pdfloc.split("/").last.substring(0,pdfloc.split("/").last.length-4)+"-"+x.caption.ID+".svg")
    )

    HTMLWriter(pdfloc.substring(0,pdfloc.length-4),pdfloc.substring(0,pdfloc.length-4)+"/"+pdfloc.split("/").last.substring(0,pdfloc.split("/").last.length-4)+"-"+"FigureTables.html")
  }

  /*This is a bit of hack. We want bounding boxes for characters, raster graphics as well as graphics paths. The algorithm to extract these
  * are different, but in the end they are just bounding boxes
  * */
  def getPathBoundingBox(c:String):Rectangle=
    try {
      if (c.startsWith("<text")&&c.endsWith(">")) {
        //println(c)
        val x = xml.XML.loadString(c).attribute("x")
        val y = xml.XML.loadString(c).attribute("y")
        (x, y) match {
          case (Some(x), Some(y)) => Rectangle(x.text.toFloat, y.text.toFloat, x.text.toFloat + 5, y.text.toFloat + 5) //TODO: Possible exception here.
          case _ => badRectangle
        }
      }
      else if (c.startsWith("<path") && c.endsWith(">")) pathBB(xml.XML.loadString(c).attribute("d"))
      else if (c.startsWith("<image") && c.endsWith(">"))
        imageBB(xml.XML.loadString(c).attribute("transform"), xml.XML.loadString(c).attribute("width"), xml.XML.loadString(c).attribute("height"))
      else badRectangle
    } catch{
      case x:org.xml.sax.SAXParseException => {println(s"SVG string parsing error: ${c.substring(0,5)}");badRectangle}
    }


  def pathBB(dString:Option[Seq[Node]]):Rectangle=
    dString match{
      case Some(dString)=> {
        val points=dString.text.toLowerCase.split("[lmcz]").map(a=>a.trim).filter(a=>a.length>0)
          .map(a=>a.split("\\s+")).map(a=>point(a(0).toFloat,a(1).toFloat)) //TODO: possible exception
        Rectangle(
          points.map(a=>a.x).min+2f,
          points.map(a=>a.y).min+2f,
          points.map(a=>a.x).max-2f,
          points.map(a=>a.y).max-2f
        )
      }
      case _ => Rectangle(0,0,0,0)
    }

  def imageBB(tmString:Option[Seq[Node]],w:Option[Seq[Node]],h:Option[Seq[Node]]):Rectangle=
    (tmString,w,h) match{
      case (Some(tmString),Some(w),Some(h)) =>
        val tmValues=tmString.text.toLowerCase.replace("matrix(","").replace(")","").split(",")
          .map(x=>x.toFloat)
        //TODO: possible exception(s)
        Rectangle(
          tmValues(4)+2f,
          tmValues(5)+2f,
          tmValues(4)+tmValues(0)*w.text.toFloat -2f,
          tmValues(5)+tmValues(3)*h.text.toFloat -2f
        )
      case _ => {println(s"error getting image bounding box from SVG"); badRectangle}
    }

  def filterRegion(r:Rectangle,ftb:Seq[FigTableTextBlock],vgt:Seq[PdfLine],vr:Seq[PdfRaster],pageBB:Rectangle):Rectangle=
    //if (List(r.x1>pageBB.x1+5f,r.y1>pageBB.y1+5f,r.x2<pageBB.x2-5f,r.y2<pageBB.y2-5f).filter(x=>x).length>=2)
    //  r
    //else
    rectMerge((ftb.map(x => x.boundingBox) ++ vgt.map(x => x.boundingBox) ++ vr.map(x => x.boundingBox)).filter(x => rectInside(x, r))) match {
      case Some(rect) => rectAllSideExtension(rect,3f,pageBB)
      case _ => rectAllSideExtension(r,3f,pageBB)
    }

}
