package pdf

import org.apache.pdfbox.pdmodel._

import org.apache.pdfbox.cos._

import java.io.File

import swing._

import javax.swing.event.{HyperlinkListener, HyperlinkEvent}

import au.ken.treeview._
import au.ken.treeview.Tree._
import au.ken.treeview.event._

import scalaj.collection.Imports._

case class PDFTreeNode(var obj: COSBase) {
  def typeString = obj match {
    case d: COSDocument   => "Document"
    case s: COSString     => "String"
    case a: COSArray      => "Array"
    case s: COSStream     => "Stream"
    case d: COSDictionary => "Dictionary"
    case b: COSBoolean    => "Boolean"
    case n: COSName       => "Name"
    case n: COSNumber     => "Number"
    case o                => o.getClass.getName
  }

  def children: Seq[PDFTreeNode] = obj match {
    case d: COSDocument   => d.getObjects.asScala map (o => new PDFTreeNode(o.getObject))
//    case a: COSArray      => a.toList.asScala map (o => new PDFTreeNode(o))
//    case d: COSDictionary => d.keySet.asScala.toList map (o => new PDFTreeNode(o))
    case _                => List()
  }

  override def toString: String = obj match {
    case o: COSObject     => o.toString
    case d: COSDocument   => d.getHeaderString + "\n" +
            new PDFTreeNode(d.getCatalog.getObject) + "\n" +
            new PDFTreeNode(d.getTrailer)
    case s: COSString     => "(" + s.getString + ")"
    case a: COSArray      => "[" + a.toList.asScala.map(o => new PDFTreeNode(o).toString).mkString(" ") + "]"
    case s: COSStream     => s.getStreamTokens.asScala mkString
    case d: COSDictionary => "<<" + d.entrySet.asScala.map(e => new PDFTreeNode(e.getKey) + " " +
              new PDFTreeNode(e.getValue)).mkString("\n") + ">>"
    case b: COSBoolean    => b.getValue.toString
    case n: COSName       => "/" + n.getName
    case i: COSInteger    => i.intValue.toString
    case n: COSNumber     => n.doubleValue.toString
    case o                => o.toString
  }
}

object Debugger extends SimpleSwingApplication {
  var doc = new PDDocument

  def top = new MainFrame {
    title = "PDFDebugger"
    menuBar = new MenuBar

    val displayWidget = new EditorPane() {//"text/html", "") {
      editable = false
      peer.addHyperlinkListener(new HyperlinkListener() {
        def hyperlinkUpdate(ev: HyperlinkEvent) {
          if (ev.getEventType() == HyperlinkEvent.EventType.ACTIVATED)
            peer.setPage(ev.getURL())
        }
      })
    }

    def readPDFFile(f: File): Unit = {
      doc.close()
      doc = PDDocument.load(f)
      val rootNode = new PDFTreeNode(doc.getDocument)
      tree.treeData = new TreeModel[PDFTreeNode](List(rootNode), _.children)
      displayWidget.text = rootNode.toString
    }
    val fileMenu = new Menu("File")
    fileMenu.contents ++= List(new MenuItem(Action("Open...") {
        val chooser = new FileChooser
        if (FileChooser.Result.Approve == chooser.showOpenDialog(contents(0)))
          readPDFFile(chooser.selectedFile)
      }),
      new MenuItem(Action("Exit") {quit()})
      )
    menuBar.contents += fileMenu

    lazy val tree = new Tree[PDFTreeNode] {
      renderer = Renderer(_.typeString)

      listenTo(selection)//, mouse.clicks) //editor, selection, mouse.clicks)

      reactions += {
        case TreePathSelected(_, _, _, newSelection, _) =>
          displayWidget.text = {
            val maybeLast = newSelection.getOrElse(List()).lastOption
            maybeLast.map(n => n.asInstanceOf[PDFTreeNode].toString) getOrElse ""
          }
      }
    }

    tree.expandAll

    val mainPanel = new BoxPanel(Orientation.Vertical)
    val centralPanel = new SplitPane(Orientation.Vertical,
      new ScrollPane(tree),
      new ScrollPane(displayWidget))
    val statusPanel = new Label("this is a statusbar")
    mainPanel.contents ++= List(centralPanel, statusPanel)
    contents = mainPanel
  }
}