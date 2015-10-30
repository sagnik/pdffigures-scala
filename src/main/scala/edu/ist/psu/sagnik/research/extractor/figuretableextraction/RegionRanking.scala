package edu.ist.psu.sagnik.research.extractor.figuretableextraction

import edu.ist.psu.sagnik.research.extractor.FigureTableExtraction.TextBlockClassification
import edu.ist.psu.sagnik.research.extractor.model.Rectangle._
import edu.ist.psu.sagnik.research.extractor.model._
/**
 * Created by szr163 on 10/20/15.
 */
object RegionRanking {

  //TODO: the extra parameter is for type erasure, I have to check for a better method sometime soon.
  def apply(extractedPdf: Seq[PdPageObject],isPDPages:Boolean): Seq[CaptionWithFigTableBB] ={
    apply(RegionExtension(extractedPdf).map(a=>CaptionAdjRegion(a.caption,a.adjRegions.copy(left=None,right=None))))
  }

  def apply(cWars: Seq[CaptionAdjRegion]):Seq[CaptionWithFigTableBB]={
    /* TODO:
        As mentioned before, our assumption is that the captions are either above or below the figure, not to the left
       or right (because we are extending caption bounding boxes to column boundaries). This ensures we can have at most two regions for a caption.
       We kept four regions so far because in future figures to the left or right of the caption can be considered. But here we will discard left or right regions.

     */
    val cWas=cWars.map(a=>CaptionAdjRegion(a.caption,a.adjRegions.copy(left=None,right=None)))
    val regionCaption=cWas.foldLeft(Seq.empty[(Region,Caption)])(
      (accum,b)=>
        b.adjRegions.top match{case Some(r) => accum:+(r,b.caption) case _=>accum}
    )++
      cWas.foldLeft(Seq.empty[(Region,Caption)])(
        (accum,b)=>
          b.adjRegions.down match{case Some(r) => accum:+(r,b.caption) case _=>accum}
      )
    val regionCaptionMap=regionCaption.groupBy(a=>a._1).map{ case(r,cs)=>(r,cs.map{case (a,c) => c })}

    if (regionCaptionMap.forall{case (r,cs) => cs.length==1}) //this is the perfect case, each
      //region has only one caption associated with it. Remember each region will have at least one caption, because of our construction.
      regionCaptionMap.map{case (r,cs) => CaptionWithFigTableBB(cs.head,r) }.toIndexedSeq
    else {
      if (regionCaptionMap.size>cWas.length) // number of regions > number of captions. Kind of lost cause. For each caption, just use one of the regions randomly.
        cWas.map(x=>CaptionWithFigTableBB(x.caption,
          Region(List(x.adjRegions.left,x.adjRegions.right,x.adjRegions.top,x.adjRegions.down).flatten.head.bb,x.caption.pageNumber)))
      else if (regionCaptionMap.size==cWas.length) // number of regions == number of captions
        pigeonHoleCaptionAssignment(regionCaptionMap,cWas)
      else  // number of regions < number of captions => regions need to be segmented to assign each caption
        regionSegmentCaptionAssignment(regionCaptionMap,cWas)
    }

  }

  def pigeonHoleCaptionAssignment(captionRegionMap:Map[Region, Seq[Caption]],cWas:Seq[CaptionAdjRegion]):Seq[CaptionWithFigTableBB]={
    val repeatedCaptions=captionRegionMap.filter(p=>p._2.length>1).values.toIndexedSeq.flatten.distinct //these are the captions that have multiple regions.
    //among all these captions, there should be atleast one caption that has a single region (by the pigeon hole principle).
    val singleRegionCaptions= repeatedCaptions.filter(p=> captionRegionMap.exists(a=> a._2.length==1 && a._2.head.equals(p)))

    iterateOverCaptions(singleRegionCaptions,repeatedCaptions,captionRegionMap,cWas)
    .map{case (r,cs) => CaptionWithFigTableBB(cs.head,r) }.toIndexedSeq
  }


  def iterateOverCaptions(srCaptions:Seq[Caption],repeatedCaptions:Seq[Caption],
                          crMap:Map[Region, Seq[Caption]],
                            cWas:Seq[CaptionAdjRegion]):Map[Region, Seq[Caption]]={
    /*
    repeatedCaptions.foreach(a=>println(s"[repeated caption content]: ${a.content}"))
    srCaptions.foreach(a=>println(s"[single caption content]: ${a.content}"))
    */
    if (repeatedCaptions.isEmpty || srCaptions.isEmpty)
      crMap
    else {
      val srCaption = srCaptions.head
      val changedCRMap=crMap.map{ case(r,cs)=> if (cs.length>1) (r,cs.filterNot(a=>a.equals(srCaption))) else (r,cs)}

      /*
      crMap.foreach(a=>println(s"[region]: ${a._1} [captions]: ${a._2.map(x=>x.ID)}"))
      println("\n------------------------------------------\n")
      changedCRMap.foreach(a=>println(s"[region]: ${a._1} [captions]: ${a._2.map(x=>x.ID)}"))
      */

      val repeatCaptions= changedCRMap.filter(p=>p._2.length>1).values.toIndexedSeq.flatten.distinct
      val sregCaptions=repeatCaptions.filter(p=> crMap.exists(a=> a._2.length==1 && a._2.head.equals(p)))

      iterateOverCaptions(sregCaptions,repeatCaptions,changedCRMap,cWas)
    }
  }
  /*
  TODO:
  This probably is wrong, we need to revisit caption region mapping sometime. Here we are just dividing the
  region with two captions into two regions.
  * */
  def regionSegmentCaptionAssignment(rcMap:Map[Region, Seq[Caption]],cWas:Seq[CaptionAdjRegion]):Seq[CaptionWithFigTableBB]={
    rcMap.foldLeft(Seq.empty[CaptionWithFigTableBB]) {
      case (accum, (r,cs)) => if (cs.length==1) accum:+CaptionWithFigTableBB(cs.head,r)
        else
        accum:+
          CaptionWithFigTableBB(cs(1),Region(Rectangle(r.bb.x1,r.bb.y1,r.bb.x2,math.round((r.bb.y1+r.bb.y2)/2)),cs(1).pageNumber)) :+
          CaptionWithFigTableBB(cs(1),Region(Rectangle(r.bb.x1,math.round(r.bb.y1+r.bb.y2/2),r.bb.x2,r.bb.y2),cs(1).pageNumber))
    }
  }



}
