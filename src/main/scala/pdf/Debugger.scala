package pdf

import org.apache.pdfbox.cos._
import org.apache.pdfbox.pdmodel._
import org.apache.pdfbox.persistence.util.COSObjectKey

import java.io.File

import swing._

import java.awt.event.KeyEvent
import java.awt.Toolkit

import javax.swing.event.{HyperlinkListener, HyperlinkEvent}

import au.ken.treeview._
import au.ken.treeview.Tree._
import au.ken.treeview.event._

import scalaj.collection.Imports._

import xml._
import javax.swing.{UIManager, KeyStroke}

case class COSXRefTable(table: java.util.Map[COSObjectKey, Integer]) extends COSBase {
  override def accept(notUsed: ICOSVisitor) = None // needed in COSBase
}

case class PDFTreeNode(obj: COSBase) {
  def typeString: String = {
    val ClassName = """.*\.COS(\w+)""".r
    val ClassName(n) = obj.getClass.getName
    n
  }

  def children: Seq[PDFTreeNode] = obj match {
    case d: COSDocument   =>
      val baseChildren = List(PDFTreeNode(d.getCatalog.getObject),
        PDFTreeNode(d.getTrailer),
        PDFTreeNode(COSXRefTable(d.getXrefTable)))
      if (d.isEncrypted)
        PDFTreeNode(d.getEncryptionDictionary) :: baseChildren
        baseChildren
    case _                => List()
  }

  def toXml: NodeSeq = {
    def obj2link(num: Long, gen: Long): NodeSeq =
      <a href={"http://" + num + "." + gen}>{num + " " + gen + " R"}</a>

    obj match {
      case o: COSObject     => obj2link(o.getObjectNumber.intValue, o.getGenerationNumber.intValue)
      case d: COSDocument   => <div>{d.getHeaderString}</div>
      case s: COSString     => <node>{"(" + s.getString + ")"}</node>
      case a: COSArray      => <node>{"["}{a.toList.asScala.map(o => <node>{PDFTreeNode(o).toXml}&nbsp;</node>)
        }{"]"}</node>
      case s: COSStream     => <node>{s.getStreamTokens.asScala}</node>
      case d: COSDictionary => <node>{"<<"}{d.entrySet.asScala.map(e => <div>{PDFTreeNode(e.getKey).toXml}&nbsp;
        {PDFTreeNode(e.getValue).toXml}</div>)}{">>"}</node>
      case b: COSBoolean    => <node>{b.getValue}</node>
      case n: COSName       => <node>{"/" + n.getName}</node>
      case i: COSInteger    => <node>{i.intValue}</node>
      case n: COSNumber     => <node>{n.doubleValue}</node>
      case x: COSXRefTable  => <node>{for ((k, v) <- x.table.asScala)
                                      yield <div>{obj2link(k.getNumber, k.getGeneration)}={v}</div>}</node>
      case o                => <node>{o}</node>
    }
  }
}

object Debugger extends SimpleSwingApplication {
  var doc = new PDDocument

  def top = new MainFrame {
    title = "PDForm"
    menuBar = new MenuBar
    minimumSize = new Dimension(800, 600)

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
      val rootNode = PDFTreeNode(doc.getDocument)
      tree.treeData = new TreeModel[PDFTreeNode](List(rootNode), _.children)
      displayWidget.text = rootNode.toXml.toString
      tree.expandAll
    }

    val fileMenu = new Menu("File")
    fileMenu.contents ++= List(new MenuItem(Action("Open...") {
        val chooser = new FileChooser
        if (FileChooser.Result.Approve == chooser.showOpenDialog(contents(0)))
          readPDFFile(chooser.selectedFile)
      }) {
        val key = KeyStroke.getKeyStroke(KeyEvent.VK_O, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask())
        peer.setAccelerator(key)
      },
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

    def openObjectLink(ev: HyperlinkEvent) {
      tree.selectPaths()
      val NumGen = """http://(\d+)\.(\d+)""".r
      val NumGen(num, gen) = ev.getURL.toString
      val obj = doc.getDocument.getObjectFromPool(new COSObjectKey(num.toLong, gen.toLong))
      displayWidget.text = PDFTreeNode(obj.getObject).toXml.toString
    }

    val mainPanel = new BoxPanel(Orientation.Vertical)
    val centralPanel = new SplitPane(Orientation.Vertical,
      new ScrollPane(tree),
      new ScrollPane(displayWidget)) { dividerLocation = 200 }
    val statusPanel = new Label("this is a statusbar")
    mainPanel.contents ++= List(centralPanel, statusPanel)
    contents = mainPanel
  }

  override def main(args: Array[String]) = {
    System.setProperty("Quaqua.tabLayoutPolicy", "wrap")
    UIManager.setLookAndFeel(ch.randelshofer.quaqua.QuaquaManager.getLookAndFeel())
    Swing.onEDT {
      startup(args)
    }
  }
}