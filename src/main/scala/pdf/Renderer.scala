package pdf

import swing._
import org.apache.pdfbox.pdmodel.PDPage
import org.apache.pdfbox.pdfviewer.PageDrawer
import java.awt.Graphics

class PDFPagePanel extends Panel {
  private val drawer = new PageDrawer
  private var page: Option[PDPage] = None
  private var pageDimension: Option[Dimension] = None

  emptyPage()
  
  def setPage(pdfPage: PDPage): Unit = {
    page = Some(pdfPage)
    pageDimension = Some(pdfPage.findMediaBox.createDimension)
    preferredSize = pageDimension.get
    background = java.awt.Color.white
  }

  def emptyPage(): Unit = {
    page = None
    pageDimension = None
    background = java.awt.Color.white
  }

  override def paint(g: Graphics2D): Unit = {
    if (page.isDefined && pageDimension.isDefined) {
      g.setColor(background)
      g.fillRect(0, 0, size.width, size.height)
      drawer.drawPage(g, page.get, pageDimension.get)
    } else emptyPage()
  }
}