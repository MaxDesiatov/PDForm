package pdf

import org.apache.pdfbox.cos._
import org.apache.pdfbox.pdmodel._
import org.apache.pdfbox.persistence.util.COSObjectKey

import java.io.File

import swing._
import event._
import Tree._

import java.awt.event.KeyEvent
import java.awt.Toolkit

import javax.swing.{UIManager, KeyStroke}
import javax.swing.event.{HyperlinkListener, HyperlinkEvent}

import collection.JavaConversions._

import xml._

case class COSXRefTable(table: java.util.Map[COSObjectKey, Integer]) extends COSBase {
  override def accept(notUsed: ICOSVisitor) = None // needed in COSBase
}

object PDFTreeNode {
  val empty = PDFTreeNode(COSNull.NULL)
}

case class PDFTreeNode(obj: COSBase,
                       var children: List[PDFTreeNode] = List()) {
  override def toString: String = {
    val ClassName = """.*\.COS(\w+)""".r
    val ClassName(n) = obj.getClass.getName
    n
  }

  def toXml: NodeSeq = {
    def obj2link(num: Long, gen: Long): NodeSeq =
      <a href={"http://" + num + "." + gen}>{num + " " + gen + " R"}</a>

    def dict2xml: NodeSeq =
      <node>{"<<"}{obj.asInstanceOf[COSDictionary].entrySet.map(e => <div>{PDFTreeNode(e.getKey).toXml}&nbsp;
        {PDFTreeNode(e.getValue).toXml}</div>)}{">>"}</node>

    obj match {
      case o: COSObject     => obj2link(o.getObjectNumber.intValue, o.getGenerationNumber.intValue)
      case d: COSDocument   => <div>{d.getHeaderString}</div>
      case s: COSString     => <node>{"(" + s.getString + ")"}</node>
      case a: COSArray      => <node>{"["}{a.toList.map(o => <node>{PDFTreeNode(o).toXml}&nbsp;</node>)}{"]"}
                              </node>
      case s: COSStream     => dict2xml ++ <node><div>stream</div>{s.getStreamTokens}<div>endstream</div></node>
      case d: COSDictionary => dict2xml
      case b: COSBoolean    => <node>{b.getValue}</node>
      case n: COSName       => <node>{"/" + n.getName}</node>
      case i: COSInteger    => <node>{i.intValue}</node>
      case n: COSNumber     => <node>{n.doubleValue}</node>
      case x: COSXRefTable  => <node>{for ((k, v) <- x.table)
                                      yield <div>{obj2link(k.getNumber, k.getGeneration)}={v}</div>}</node>
      case n: COSNull       => <node>null</node>
      case o                => <node>{o}</node>
    }
  }
}

object Debugger extends SimpleSwingApplication {
  var doc = new PDDocument
  var rootNode = PDFTreeNode.empty
  var currNode = PDFTreeNode.empty
  var objCache = Map.empty[(Long, Long), Path[PDFTreeNode]]

  def top = new MainFrame {
    title = "PDForm"
    menuBar = new MenuBar
    minimumSize = new Dimension(800, 600)

    val displayScrollPane = new ScrollPane {
      verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
    }
    val displayWidget = new EditorPane ("text/html", "") {
      editable = false
      peer.addHyperlinkListener(new HyperlinkListener() {
        def hyperlinkUpdate(ev: HyperlinkEvent) =
          if (ev.getEventType() == HyperlinkEvent.EventType.ACTIVATED)
            openObjectLink(ev)
      })
    }

    def readPDFFile(f: File): Unit = {
      doc.close()
      doc = PDDocument.load(f)
      val docObj = doc.getDocument
      rootNode = PDFTreeNode(docObj)
      val baseChildren = List(PDFTreeNode(docObj.getCatalog.getObject),
                              PDFTreeNode(docObj.getTrailer),
                              PDFTreeNode(COSXRefTable(docObj.getXrefTable)))
      rootNode.children = if (docObj.isEncrypted)
                            PDFTreeNode(docObj.getEncryptionDictionary) :: baseChildren
                          else
                            baseChildren
      tree.treeData = new TreeModel[PDFTreeNode](List(rootNode), _.children)
      tree.expandAll
      tree.selectRows(0)
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
      listenTo(selection)

      reactions += {
        case TreePathSelected(_, _, _, newSelection, _) =>       
          displayWidget.text = {
            val maybeLast = newSelection.getOrElse(List()).lastOption.asInstanceOf[Option[PDFTreeNode]]
            currNode = maybeLast getOrElse PDFTreeNode.empty
            maybeLast.map(n => n.toXml.toString) getOrElse ""
          }
          displayWidget.peer.select(0, 0)
      }
    }

    def openObjectLink(ev: HyperlinkEvent) {
      val NumGen = """http://(\d+)\.(\d+)""".r
      val NumGen(numStr, genStr) = ev.getURL.toString
      val (num, gen) = (numStr.toLong, genStr.toLong)
      val newPath = objCache.getOrElse((num, gen), {
        val obj = PDFTreeNode(doc.getDocument.getObjectFromPool(new COSObjectKey(num, gen)).getObject)
        currNode.children ::= obj
        currNode = obj
        val sel = tree.treePathToPath(tree.selection.paths.leadSelection) :+ obj
        tree.treeData = new TreeModel[PDFTreeNode](List(rootNode), _.children)
        objCache += (num, gen) -> sel
        sel
      })
      tree.expandAll
      tree.selectPaths(newPath)
    }

    val mainPanel = new BoxPanel(Orientation.Vertical)
    displayScrollPane.contents = displayWidget
    val centralPanel = new SplitPane(Orientation.Vertical,
                                     new ScrollPane(tree),
                                     displayScrollPane) {
      dividerLocation = 200
    }
    val statusPanel = new Label("this is a statusbar")
    mainPanel.contents ++= List(centralPanel, statusPanel)
    contents = mainPanel
  }

  override def main(args: Array[String]) = {
    if ("""Windows""".r.findFirstIn(System.getProperty("os.name")).isEmpty) {
        System.setProperty("Quaqua.tabLayoutPolicy", "wrap")
        UIManager.setLookAndFeel(ch.randelshofer.quaqua.QuaquaManager.getLookAndFeel())
      }
    else
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName())

    Swing.onEDT {
      startup(args)
    }
  }
}
  