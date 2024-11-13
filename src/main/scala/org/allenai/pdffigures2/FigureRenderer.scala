package org.allenai.pdffigures2

import org.apache.pdfbox.pdmodel.PDDocument
import spray.json._

import java.awt.image.BufferedImage
import java.io._
import javax.imageio.ImageIO
import scala.annotation.unused

/** Methods rendering figures as images and saving those images to disk */
object FigureRenderer {

  val CairoFormat: Set[String] = Set("ps", "eps", "pdf", "svg")
  val AllowedFormats: Set[String] = CairoFormat ++ ImageIO.getWriterFormatNames

  /** Maximum pixels to expand rasterized figure when cleaning */
  private val MaxExpand = 20

  /** Min space to allow between expanded regions and non-figure content when cleaning */
  private val PadNonFigureContent = 2

  /** If we don't expand the image, how much to pad the figure region when it is rasterized */
  private val PadUnexpandedImage = 1

  /** Expands the given region in an attempt to ensure no connected components in `img` are
    * only partially contained in the region, while ensuring region does not
    * intersect any box in `otherContentScaled`.
    *
    * This used because we can get slightly incorrect bounding boxes from PDFBox, so the bounding
    * boxes might clip/crop out things like vertical text. This method attempts to detect cases
    * where elements have been clipped in this way and expand the input bounding box so include the
    * cropped element.
    *
    * @param otherContentScaled regions, in the same scale/DPI as `img`, to avoid intersecting
    * @param contentPad required distance the expansion is from any box in `otherContentScaled`
    * @param img Image to expand the region on
    * @return The expanded region in pixel, inclusive coordinates of the form (x1, y1, x2, y2)
    */
  def expandFigureBounds(
    x1: Int,
    y1: Int,
    x2: Int,
    y2: Int,
    otherContentScaled: Seq[Box],
    contentPad: Int,
    img: BufferedImage
  ): (Int, Int, Int, Int) = {
    val h = img.getHeight
    val w = img.getWidth
    var newY1 = y1
    var newX1 = x1
    var newY2 = y2
    var newX2 = x2

    def isColored(x: Int, y: Int): Boolean = img.getRGB(x, y) != 0xffffffff
    def intersectsAny(x1: Int, y1: Int, x2: Int, y2: Int): Boolean =
      Box(x1, y1, x2, y2).intersectsAny(otherContentScaled, contentPad)

    // Expand while 1) we aren't at a border 2) we haven't exceeded `MaxExpand`
    // 3) we won't come close to any other content and
    // 4) doing so adds a non-empty pixel connected to an existing non-empty pixel
    while (newY1 > 0 && y1 - newY1 < MaxExpand &&
           !intersectsAny(newX1, newY1 - 1, newX2, newY1) &&
           Range(newX1, newX2 + 1).exists(x => isColored(x, newY1 - 1) && isColored(x, newY1))) {
      newY1 -= 1
    }
    while (newY2 < h - 1 && newY2 - y2 < MaxExpand &&
           !intersectsAny(newX1, newY2, newX2, newY2 + 1) &&
           Range(newX1, newX2 + 1).exists(x => isColored(x, newY2 + 1) && isColored(x, newY2))) {
      newY2 += 1
    }
    while (newX1 > 0 && x1 - newX1 < MaxExpand &&
           !intersectsAny(newX1 - 1, newY1, newX1, newY2) &&
           Range(newY1, newY2 + 1).exists(y => isColored(newX1, y) && isColored(newX1 - 1, y))) {
      newX1 -= 1
    }
    while (newX2 < w - 1 && newX2 - x2 < MaxExpand &&
           !intersectsAny(newX2, newY1, newX2 + 1, newY2) &&
           Range(newY1, newY2 + 1).exists(y => isColored(newX2, y) && isColored(newX2 + 1, y))) {
      newX2 += 1
    }
    (newX1, newY1, newX2, newY2)
  }

  /** Rasterize the figures in `page` with `dpi` and optionally cleaning/post-processing the figure
    * regions
    */
  def rasterizeFigures(
    doc: PDDocument,
    page: PageWithFigures,
    dpi: Int,
    clean: Boolean,
    logger: Option[VisualLogger]
  ): Seq[RasterizedFigure] = {
    val scale = dpi / 72.0
    val nonFigureContent =
      ((page.classifiedText.allText ++ page.paragraphs).map(_.boundary) ++
        page.failedCaptions.map(_.boundary) ++
        page.figures.map(_.captionBoundary)).map(_.scale(scale))
    var figureRegions = page.figures.map(_.regionBoundary).map(_.scale(scale))
    val renderer = new InterruptiblePDFRenderer(doc)
    val pageImg = renderer.renderImageWithDPI(page.pageNumber, dpi.toFloat)
    val rasterized = page.figures.zipWithIndex.map {
      case (fig, figureNumber) =>
        val otherFigureRegions =
          figureRegions.take(figureNumber) ++ figureRegions.drop(figureNumber + 1)
        val r = fig.regionBoundary
        val x1 = Math.max(Math.floor(scale * r.x1).toInt, 0)
        val y1 = Math.max(Math.floor(scale * r.y1).toInt, 0)
        val x2 = Math.min(Math.ceil(scale * r.x2).toInt, pageImg.getWidth - 1)
        val y2 = Math.min(Math.ceil(scale * r.y2).toInt, pageImg.getHeight - 1)
        val (cx1, cy1, cx2, cy2) = if (clean) {
          expandFigureBounds(
            x1,
            y1,
            x2,
            y2,
            nonFigureContent ++ otherFigureRegions,
            PadNonFigureContent,
            pageImg
          )
        } else {
          (
            Math.max(x1 - PadUnexpandedImage, 0),
            Math.max(y1 - PadUnexpandedImage, 0),
            Math.min(x2 + PadUnexpandedImage * 2, pageImg.getWidth - 1),
            Math.min(y2 + PadUnexpandedImage * 2, pageImg.getHeight - 1)
          )
        }
        val figureImage = pageImg.getSubimage(cx1, cy1, cx2 - cx1 + 1, cy2 - cy1 + 1)
        val rasterizedFigure = RasterizedFigure(fig, Box(cx1, cy1, cx2, cy2), figureImage, dpi)
        figureRegions = figureRegions.updated(figureNumber, fig.regionBoundary)
        rasterizedFigure
    }
    if (logger.isDefined) logger.get.logRasterizedFigures(page.pageNumber, rasterized)
    rasterized
  }

  /** Save rasterized figures in the given image format */
  def saveRasterizedFigures(
    figuresAndFilenames: Seq[(String, RasterizedFigure)],
    format: String,
    @unused dpi: Int
  ): Seq[SavedFigure] = {
    require(ImageIO.getWriterFormatNames.contains(format), s"Can't save to format $format")
    figuresAndFilenames.map {
      case (filename, rasterizedFigure) =>
        ImageIO.write(rasterizedFigure.bufferedImage, format, new File(filename))
        SavedFigure(rasterizedFigure, filename)
    }
  }

  /** Save figures to disk in a vector graphic format by shelling out to pdftocairo */
  def saveFiguresAsImagesCairo(
                                doc: PDDocument,
                                figuresAndFilenames: Seq[(String, Figure)],
                                format: String,
                                dpi: Int
                              ): Iterable[SavedFigure] = {
    require(CairoFormat.contains(format), s"Cairo can't render to format $format")
    val groupedByPage = figuresAndFilenames.groupBy(_._2.page)
    groupedByPage.flatMap { case (pageNum, pageFigures) =>
      val pageDoc = new PDDocument() // Save some IO by just sending cairo the relevant page
      pageDoc.addPage(doc.getPage(pageNum))
      val savedFigures = pageFigures.map { case (filename, fig) =>
        if (Thread.interrupted()) throw new InterruptedException()
        val box = fig.regionBoundary
        val x = Math.round(box.x1) - PadUnexpandedImage
        val y = Math.round(box.y1) - PadUnexpandedImage
        val w = Math.round(box.width) + PadUnexpandedImage * 2
        val h = Math.round(box.height) + PadUnexpandedImage * 2

        // Build the process with explicit arguments instead of shell string
        val processBuilder = new ProcessBuilder(
          "pdftocairo",
          s"-$format",
          "-r", dpi.toString,
          "-x", x.toString,
          "-y", y.toString,
          "-H", h.toString,
          "-W", w.toString,
          "-paperw", w.toString,
          "-paperh", h.toString,
          "-",  // Read from stdin
          filename
        )

        // Start the process
        val process = processBuilder.start()

        // Get output stream and save document
        val outStream = process.getOutputStream
        pageDoc.save(outStream) // Stream the doc to cairo
        outStream.close() // Important to close the stream

        // Wait for process to complete and check exit code
        if (process.waitFor() != 0) {
          // Optionally, you could read error stream here for more detailed error message
          val errorStream = scala.io.Source.fromInputStream(process.getErrorStream).mkString
          throw new IOException(s"Error using cairo to save figure: $errorStream")
        }

        SavedFigure(fig, filename, dpi)
      }
      pageDoc.close()
      savedFigures
    }
  }

  def saveAsJSON[T: JsonFormat](outputFilename: String, toSave: T): Unit = {
    val file = new File(outputFilename)
    val writer = new PrintWriter(file)
    writer.write(toSave.toJson.prettyPrint)
    writer.close()
  }
}
