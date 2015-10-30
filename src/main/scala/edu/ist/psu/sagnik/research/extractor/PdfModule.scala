package edu.ist.psu.sagnik.research.extractor

import java.awt.image.BufferedImage
import edu.ist.psu.sagnik.research.extractor.model.PdPageObject
import scala.language.existentials

/**
 * Type module for actions on PDFs.
 *
 * Notable function types are `Convert` and `Rasterize`, which extract the text
 * per-page and generate images per-page, respectively.
 */

trait PdfModule[A] {
  type Pdf = A

  type Content = Seq[String]
  type Convert = Pdf => Content

  type Raster = Seq[BufferedImage]
  type Rasterize = Pdf => Raster

  type Object = PdPageObject
  type Extractor = Pdf => Object


}

/**
 * Generic helper functions for PdfModule types and instantiations alike.
 */
object PdfModule {

  /**
   * Constructs a single string from the content `c`, joining elements
   * using the delimiter `delim`.
   */
  def mkString(c: PdfModule[A forSome { type A }]#Content, delim: String): String =
    c.mkString(delim)

  /**
   * Equivalent to mkString where delim == "\n"
   */
  def mkString(c: PdfModule[A forSome { type A }]#Content): String =
    mkString(c, "\n")

  /**
   * Removes non-alphanumeric characters and collapses multiple whitespace characters.
   */
  def clean(s: String): String =
    s
      // get rid of non-words
      .replaceAll("[^\\w ]", "")
      // get rid of multiple whitespaces
      .replaceAll("\\s+", " ")

  /**
   * Wraps a Converter type. Will apply `clean` to each element for the
   * evaluated Content.
   */
  def cleaner[A](converter: PdfModule[A]#Convert): PdfModule[A]#Convert =
    (pdf: PdfModule[A]#Pdf) =>
      converter(pdf).map(clean)

}