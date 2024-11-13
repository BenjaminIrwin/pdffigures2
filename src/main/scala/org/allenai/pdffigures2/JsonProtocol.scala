package org.allenai.pdffigures2

import org.allenai.pdffigures2.FigureExtractor.Document
import org.allenai.pdffigures2.FigureExtractor.DocumentWithSavedFigures
import org.allenai.pdffigures2.SectionedTextBuilder.DocumentSection
import org.allenai.pdffigures2.SectionedTextBuilder.PdfText
import spray.json._

class FigureTypeJsonConverter extends RootJsonFormat[FigureType.Value] {
  override def write(obj: FigureType.Value): JsValue = JsString(obj.toString)

  override def read(json: JsValue): FigureType.Value = {
    json match {
      case JsString(txt) => FigureType.withName(txt)
      case somethingElse => throw DeserializationException(s"Expected a value from enum FigureType instead of $somethingElse")
    }
  }
}


trait JsonProtocol extends DefaultJsonProtocol {
  // JSON formats so we can write Figures/Captions/Documents to disk
  implicit val enumConverter: RootJsonFormat[FigureType.Value] = new FigureTypeJsonConverter()
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
