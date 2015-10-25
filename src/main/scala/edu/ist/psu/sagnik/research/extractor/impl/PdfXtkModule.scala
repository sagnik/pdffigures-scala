package edu.ist.psu.sagnik.research.extractor.impl

import java.io.File

import at.ac.tuwien.dbai.pdfwrap.ProcessFile
import at.ac.tuwien.dbai.pdfwrap.model.document._
import edu.ist.psu.sagnik.research.extractor.model._

object PdfXtkModule {

  type PdfXtkDocument = Seq[Page]

  object Load {

    def apply(pdfFileLocation: String): PdfXtkDocument =
      apply(new File(pdfFileLocation))

    import scala.collection.JavaConversions._

    def apply(pdfFile: File): PdfXtkDocument =
      ProcessFile.getPageObjects(pdfFile.getAbsolutePath)
  }

  implicit val extractor = PdfxTkExtractor
}

/**
 * Prints to stdout each page's graphics paths, text lines, and paragraphs.
 * Includes bounding box information for each aforementioned PDF object.
 */
object PdfObjectPrettyPrint {

  def apply(pdfObjectsAllPages: Seq[PdPageObject]): Unit =
    pdfObjectsAllPages.zipWithIndex
      .foreach {
        case (pdfObjectThisPage, pageNumber) =>
          print(s"page number $pageNumber\n----------------\n")

          pdfObjectThisPage
            .pdChars.zipWithIndex
            .foreach {
            case (char, number) =>
              print(s"char no. $number |char bounding box: ${char.boundingBox} |char content: ${char.content}\n")
          }
          print("\n-----------------------\n")

          pdfObjectThisPage
            .pdLines.zipWithIndex
            .foreach {
              case (line, number) =>
                print(s"graphics path no. $number |path bounding box: ${line.boundingBox}\n")
            }
          print("\n-----------------------\n")

          pdfObjectThisPage
            .pdTextLines.zipWithIndex
            .foreach {
              case (textLine, number) =>
                print(s"\nline no. $number |line bounding box: ${textLine.boundingBox} |line content: ${textLine.content}")
          }

          print("\n-----------------------\n")


          pdfObjectThisPage
            .pdParagraphs.zipWithIndex
            .foreach {
              case (paragraph, number) =>
                print(s"\npargraph no. $number |paragraph bounding box: ${paragraph.boundingBox} |pargraph content: ${paragraph.content}")
            }
          print("\n-----------------------\n")

      }
}