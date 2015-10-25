package edu.ist.psu.sagnik.research.extractor.captionmentionextraction


/**
 * Created by szr163 on 9/24/15.
 */
object CaptionMention {

  object Load {

    import CaptionMentionExtractionHelper._

    def apply(pdLines:Seq[A], pdPargraphs:Seq[B], pdGraphics:Seq[C], pdImages:Seq[D], colMarginXs:Seq[Float]) = getCaptions(pdLines, pdPargraphs, pdGraphics, pdImages, colMarginXs)

    def getCaptions(pdLines:Seq[A], pdParagraphs:Seq[B], pdGraphics:Seq[C], pdImages:Seq[D], colMarginXs:Seq[Float]): captions = {
      pdLines.
        filter(p => p.content.trim().startsWith("Fig") || p.content.trim().startsWith("Table"))
        .flatMap(p => convertToCaption(p, pdParagraphs, pdGraphics, pdImages, colMarginXs))
        .filter(p=>p.confidence>0)
        .map(a => checkForParagraphs(a, pdLines)).
        groupBy(p => p.ID).values.
        map(a => a.toIndexedSeq.sortWith(_.confidence > _.confidence)(0))
        .toIndexedSeq

    }
  }
}


