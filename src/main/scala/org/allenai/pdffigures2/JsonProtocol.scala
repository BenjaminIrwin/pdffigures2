package org.allenai.pdffigures2

import org.allenai.pdffigures2.FigureExtractor.Document
import org.allenai.pdffigures2.FigureExtractor.DocumentWithSavedFigures
import org.allenai.pdffigures2.SectionedTextBuilder.DocumentSection
import org.allenai.pdffigures2.SectionedTextBuilder.PdfText
import spray.json._

// From https://github.com/spray/spray-json/issues/200
// to support enum -> json conversion
class EnumJsonConverter[T <: scala.Enumeration](enu: T) extends RootJsonFormat[T#Value] {
  override def write(obj: T#Value): JsValue = JsString(obj.toString)

  override def read(json: JsValue): T#Value = {
    json match {
      case JsString(txt) => enu.withName(txt)
      case somethingElse => throw DeserializationException(s"Expected a value from enum $enu instead of $somethingElse")
    }
  }
}

trait JsonProtocol extends DefaultJsonProtocol {
  // JSON formats so we can write Figures/Captions/Documents to disk
  implicit val enumConverter: EnumJsonConverter[FigureType.type] = new EnumJsonConverter(FigureType)
  implicit val boxFormat: RootJsonFormat[Box] = jsonFormat4(Box.apply)
  implicit val captionFormat: RootJsonFormat[Caption] = jsonFormat5(Caption.apply)
  implicit val figureFormat: RootJsonFormat[Figure] = jsonFormat7(Figure.apply)
  implicit val savedFigureFormat: RootJsonFormat[SavedFigure] = jsonFormat9(SavedFigure.apply)
  implicit val documentTextFormat: RootJsonFormat[PdfText] = jsonFormat3(PdfText.apply)
  implicit val documentSectionFormat: RootJsonFormat[DocumentSection] = jsonFormat2(DocumentSection.apply)
  implicit val documentFormat: RootJsonFormat[Document] = jsonFormat3(Document.apply)
  implicit val documentWithFiguresFormat: RootJsonFormat[DocumentWithSavedFigures] = jsonFormat3(DocumentWithSavedFigures.apply)
}

object JsonProtocol extends JsonProtocol
