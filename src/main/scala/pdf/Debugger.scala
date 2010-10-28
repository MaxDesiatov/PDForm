package pdf

import org.apache.pdfbox.cos._
import org.apache.pdfbox.pdmodel._
import org.apache.pdfbox.persistence.util.COSObjectKey

import java.io.File

import swing._

import javax.swing.event.{HyperlinkListener, HyperlinkEvent}

import au.ken.treeview._
import au.ken.treeview.Tree._
import au.ken.treeview.event._

import scalaj.collection.Imports._

import xml._

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

  def toXml: NodeSeq = obj match {
    case o: COSObject     =>
      val num = o.getObjectNumber.intValue
      val gen = o.getGenerationNumber.intValue
      <a href={"http://" + num + "." + gen}>{num + " " + gen + " R"}</a>
    case d: COSDocument   => <div>{d.getHeaderString}</div>
            <div>{new PDFTreeNode(d.getCatalog.getObject).toXml}</div>
            <div>{new PDFTreeNode(d.getTrailer).toXml}</div>
    case s: COSString     => <node>{"(" + s.getString + ")"}</node>
    case a: COSArray      => <node>{"["}{a.toList.asScala.map(o => <node>{new PDFTreeNode(o).toXml}&nbsp;</node>)
      }{"]"}</node>
    case s: COSStream     => <node>{s.getStreamTokens.asScala}</node>
    case d: COSDictionary => <node>{"<<"}{d.entrySet.asScala.map(e => <div>{new PDFTreeNode(e.getKey).toXml}&nbsp;
      {new PDFTreeNode(e.getValue).toXml}</div>)}{">>"}</node>
    case b: COSBoolean    => <node>{b.getValue}</node>
    case n: COSName       => <node>{"/" + n.getName}</node>
    case i: COSInteger    => <node>{i.intValue}</node>
    case n: COSNumber     => <node>{n.doubleValue}</node>
    case o                => <node>{o}</node>
  }
}

object Debugger extends SimpleSwingApplication {
  var doc = new PDDocument

  def top = new MainFrame {
    title = "PDFDebugger"
    menuBar = new MenuBar

    val displayWidget = new EditorPane ("text/html", "") {
      editable = false
      peer.addHyperlinkListener(new HyperlinkListener() {
        def hyperlinkUpdate(ev: HyperlinkEvent) =
          if (ev.getEventType() == HyperlinkEvent.EventType.ACTIVATED) openObjectLink(ev)
      })
    }

    def readPDFFile(f: File): Unit = {
      doc.close()
      doc = PDDocument.load(f)
      val rootNode = new PDFTreeNode(doc.getDocument)
      tree.treeData = new TreeModel[PDFTreeNode](List(rootNode), _.children)
      displayWidget.text = rootNode.toXml.toString
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
            maybeLast.map(n => n.asInstanceOf[PDFTreeNode].toXml.toString) getOrElse ""
          }
      }
    }

    tree.expandAll

    def openObjectLink(ev: HyperlinkEvent) {
      tree.selectPaths()
      val num :: gen :: _ = "http://".r.replaceAllIn(ev.getURL.toString, "").split('.').toList
      val obj = doc.getDocument.getObjectFromPool(new COSObjectKey(num.toLong, gen.toLong))
      displayWidget.text = new PDFTreeNode(obj.getObject).toXml.toString
    }

    val mainPanel = new BoxPanel(Orientation.Vertical)
    val centralPanel = new SplitPane(Orientation.Vertical,
      new ScrollPane(tree),
      new ScrollPane(displayWidget))
    val statusPanel = new Label("this is a statusbar")
    mainPanel.contents ++= List(centralPanel, statusPanel)
    contents = mainPanel
  }
}