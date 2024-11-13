package org.allenai.pdffigures2

import org.apache.pdfbox.contentstream.operator.Operator
import org.apache.pdfbox.cos.COSBase
import org.apache.pdfbox.pdmodel.PDDocument
import org.apache.pdfbox.rendering.PDFRenderer
import org.apache.pdfbox.rendering.PageDrawer
import org.apache.pdfbox.rendering.PageDrawerParameters

class InterruptiblePDFRenderer(doc: PDDocument) extends PDFRenderer(doc) {

  class InterruptiblePageDrawer(param: PageDrawerParameters) extends PageDrawer(param) {
    override def processOperator(operator: Operator, operands: java.util.List[COSBase]): Unit = {
      if (Thread.interrupted()) throw new InterruptedException()
      super.processOperator(operator, operands)
    }
  }

  override def createPageDrawer(parameters: PageDrawerParameters): PageDrawer = {
    new InterruptiblePageDrawer(parameters)
  }
}
