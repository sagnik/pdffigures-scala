package edu.ist.psu.sagnik.research.extractor.impl

import edu.ist.psu.sagnik.research.extractor.model.Rectangle
import org.apache.pdfbox.pdmodel.{PDPage, PDDocument}
import org.apache.pdfbox.util.PDFTextStripperByArea
import edu.ist.psu.sagnik.research.extractor.model.Rectangle._
/**
 * Created by sagnik on 10/29/15.
 */
object PdfBoxExtractor {

  /*
* For some reason pdfXtk jumbles up all contents in a text block, therefore,
* we extract the "proper" caption using PDFTextStripperbyArea.
* */
  def changeRectContent(content:String,cr:Rectangle,p:Int, pdfloc:String,pageBB:Rectangle,pdfXtkCrSystem:Boolean,isCaptionRect:Boolean):String={
    val doc = PDDocument.load(pdfloc)

    val changedContent=
    try {
      val page = doc.getDocumentCatalog.getAllPages.get(p - 1).asInstanceOf[PDPage] //TODO: has changed in recent snapshots, also, possible exception
      val tolerance=if (isCaptionRect) 5f else 1f
      val rect = if (!pdfXtkCrSystem)
        new java.awt.geom.Rectangle2D.Float(cr.x1 - tolerance, cr.y1 - tolerance, cr.x2 - cr.x1 + 2*tolerance, cr.y2 - cr.y1 + 2*tolerance)
      else
        new java.awt.geom.Rectangle2D.Float(cr.x1 - tolerance,pageBB.y2 - cr.y2 - tolerance,cr.x2 - cr.x1 + 2*tolerance, cr.y2 - cr.y1 + 2*tolerance)
      println(s"[content] ${asCoordinatesStr(cr)}:::: "+content+s" :::: ${rect}")
      val stripper = new PDFTextStripperByArea()
      stripper.addRegion("cropbox", rect)
      stripper.setSortByPosition(true)
      stripper.extractRegions(page);
      stripper.getTextForRegion("cropbox")
    }
    catch {case e:Exception => content}
    finally doc.close()
    println("[changed content]: "+changedContent)
    if (changedContent.trim.isEmpty) content else changedContent.split("\n").head
  }

}
