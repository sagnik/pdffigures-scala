package edu.ist.psu.sagnik.research.extractor.model

import at.ac.tuwien.dbai.pdfwrap.model.document._

/**
 * A quadrilateral, defined by its bottom-left point (x1,y1)
 * and its top-right point (x2, y2).
 */
case class Rectangle(x1: Float, y1: Float, x2: Float, y2: Float)

/**
 * Methods for constructing or otherwise manipulating Rectangle instances.
 */
object Rectangle {

  implicit def round(x:Float)=scala.math.round(x)

  /**
   * Evaluates to a string that contains the Rectangle's
   * point information as x1,y2,x2,y2 .
   */
  def asCoordinatesStr(r: Rectangle): String =
    s"${r.x1},${r.y1},${r.x2},${r.y2}"

  def fromLineSegment(ls: LineSegment): Rectangle =
    Rectangle(round(ls.getX1), round(ls.getY1), round(ls.getX2), round(ls.getY2))

  def fromRectSegment(rs: RectSegment): Rectangle =
    Rectangle(round(rs.getX1), round(rs.getY1), round(rs.getX2), round(rs.getY2))

  def fromImageSegment(is: ImageSegment): Rectangle =
    Rectangle(round(is.getX1), round(is.getY1), round(is.getX2), round(is.getY2))

  def fromTextBlock(tb: TextBlock): Rectangle =
    Rectangle(round(tb.getX1), round(tb.getY1), round(tb.getX2), round(tb.getY2))

  def fromTextLine(tl: TextLine): Rectangle =
    Rectangle(round(tl.getX1), round(tl.getY1), round(tl.getX2), round(tl.getY2))

  def fromCharSegment(cs: CharSegment): Rectangle =
    Rectangle(round(cs.getX1), round(cs.getY1), round(cs.getX2), round(cs.getY2))

  def rectInterSects(r1:Rectangle,r2:Rectangle):Boolean={
    if (r1.equals(r2)) true
    else
      r1!=r2 && r1.x1 <= r2.x2 && r2.x1 <= r1.x2 && r1.y1 <= r2.y2 && r2.y1 <= r1.y2
  }

  //TODO:check correctness
  def rectInside(in:Rectangle,out:Rectangle):Boolean= in.x1>=out.x1 && in.y1>=out.y1 && in.x2<=out.x2 && in.y2<=out.y2

  def rectInside(in:Rectangle,out:Rectangle,tolerance:Float):Boolean= in.x1+tolerance>=out.x1 && in.y1+tolerance>=out.y1 && in.x2-tolerance<=out.x2 && in.y2-tolerance<=out.y2

  implicit def min(x:Float,y:Float)=scala.math.min(x,y)
  implicit def max(x:Float,y:Float)=scala.math.max(x,y)

  def rectAllSideExtension(r1:Rectangle,param:Float,maxRect:Rectangle):Rectangle=
    Rectangle(max(maxRect.x1,r1.x1-param),max(maxRect.y1,r1.y1-param),min(maxRect.x2,r1.x2+param),min(maxRect.y2,r1.y2+param))

  def rectBothYExtension(r1:Rectangle,param:Float,maxRect:Rectangle):Rectangle=
    Rectangle(r1.x1,max(maxRect.y1,r1.y1-param),r1.x2,min(maxRect.y2,r1.y2+param))

  def rectBothXExtension(r1:Rectangle,param:Float,maxRect:Rectangle):Rectangle=
    Rectangle(max(maxRect.x1,r1.x1-param),r1.y1,min(maxRect.x2,r1.x2+param),r1.y2)



  def rectMerge(r1:Rectangle,r2:Rectangle):Rectangle=
    Rectangle(min(r1.x1,r2.x1),min(r1.y1,r2.y1),max(r1.x2,r2.x2),max(r1.y2,r2.y2))

  def rectMerge(rs:Seq[Rectangle]):Option[Rectangle]=
    if (rs.isEmpty) None
    else Some(
      Rectangle(
        rs.map(a=>a.x1).min,
        rs.map(a=>a.y1).min,
        rs.map(a=>a.x2).max,
        rs.map(a=>a.y2).max
      )
    )

  //a bit of caution: this works only for axes parallel rectangles.
  // That suffice for our purpose, but this isn't a generic method.
  def rectDistance(r1:Rectangle,r2:Rectangle):Float={
    val dy1=if (r1.y2<r2.y1) r2.y1-r1.y2 else 0
    val dy2=if (r1.y1>r2.y2) r1.y1-r2.y2 else 0
    val dx1=if (r1.x2<r2.x1) r2.x1-r1.x2 else 0
    val dx2=if (r1.x1>r2.x2) r1.x1-r2.x2 else 0
    dx1+dx2+dy1+dy2
  }

  def rectExtend(r:Rectangle,rs:Seq[Rectangle],checkAgainstRS:Seq[Rectangle],pageBB:Rectangle,direction:String):Option[Rectangle]=
    if (
      !rs.exists(a=>rectInterSects(a,r)) && //haven't found any intersection with body or caption text boxes
        (r.x1>pageBB.x1 && r.x2<pageBB.x2 && r.y1>pageBB.y1 && r.y2 < pageBB.y2) // all coordinates OK with pageBB
    )
      rectExtend(changeRect(r,1f,direction),rs,checkAgainstRS,pageBB,direction)
    else {
      if (checkAgainstRS.filter(a=>rectInterSects(Rectangle(r.x1+2,r.y1+2,r.x2-2,r.y2-2),a)).length>2 //we have got an extended rectangle.
        // Need to see if this is empty, or has a very small size
        && (r.x2-r.x1>10f && r.y2-r.y1> 10f)
      )
        Some(changeRect(r,-1f,direction))
      else
        None
    }

  def changeRect(r:Rectangle,changeparam:Float,direction:String):Rectangle=
    direction match {
      case "left" => r.copy(x1 = r.x1 - changeparam)
      case "right" => r.copy(x2 = r.x2 + changeparam)
      case "top" => r.copy(y2 = r.y2 + changeparam)
      case "down" => r.copy(y1 = r.y1 - changeparam)
      case _ => r
    }


  /*
  TODO: test these methods
  * */
  def rectVerticalDistance(r1:Rectangle,r2:Rectangle):Float={
    val dy1=if (r1.y2<r2.y1) r2.y1-r1.y2 else 0
    val dy2=if (r1.y1>r2.y2) r1.y1-r2.y2 else 0
    dy1+dy2
  }

  def rectHorizontalDistance(r1:Rectangle,r2:Rectangle):Float={
    val dx1=if (r1.x2<r2.x1) r2.x1-r1.x2 else 0
    val dx2=if (r1.x1>r2.x2) r1.x1-r2.x2 else 0
    dx1+dx2
  }

}