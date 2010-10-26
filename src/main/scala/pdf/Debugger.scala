package pdf

import org.apache.pdfbox.exceptions.InvalidPasswordException

import org.apache.pdfbox.pdfviewer.PDFTreeModel
import org.apache.pdfbox.pdfviewer.PDFTreeCellRenderer
import org.apache.pdfbox.pdfviewer.ArrayEntry
import org.apache.pdfbox.pdfviewer.MapEntry

import org.apache.pdfbox.pdmodel.PDDocument

import org.apache.pdfbox.util.ExtensionFileFilter

import org.apache.pdfbox.cos.COSBoolean
import org.apache.pdfbox.cos.COSFloat
import org.apache.pdfbox.cos.COSInteger
import org.apache.pdfbox.cos.COSName
import org.apache.pdfbox.cos.COSNull
import org.apache.pdfbox.cos.COSStream
import org.apache.pdfbox.cos.COSString

import javax.swing.tree.TreeModel
import javax.swing.tree.TreePath
import javax.swing.JFileChooser
import javax.swing.JScrollPane
import javax.swing.JPanel
import javax.swing.UIManager

import java.io.ByteArrayOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.io.IOException

import swing._
import swing.event._

object Debugger extends SimpleSwingApplication {
  val click = new Button {
      text = "click"
    }
  def top = new MainFrame {
    title = "PDFDebugger"
    contents = new FlowPanel(click)
  }
  listenTo(click)
  reactions += {
    case ButtonClicked(`click`) => quit()
  }
}