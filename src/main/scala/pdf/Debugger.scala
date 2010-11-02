package pdf

import org.apache.pdfbox.cos._
import org.apache.pdfbox.pdmodel._
import org.apache.pdfbox.util.PDFOperator
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

  implicit def COSBase2PDFTreeNode(cos: COSBase): PDFTreeNode = PDFTreeNode(cos)

  def time(f: => Unit): String = {
    val t1 = System.currentTimeMillis()
    f
    val t2 = System.currentTimeMillis()
    "%d msecs" format (t2 - t1)
  }

  def token2xml(t: Any): NodeSeq = {
    def obj2link(num: Long, gen: Long): NodeSeq =
      <a href={"http://" + num + "." + gen}>{num + " " + gen + " R"}</a>

    def dict2xml(d: COSDictionary): NodeSeq =
      Text("<<") ++ d.asInstanceOf[COSDictionary].entrySet.
              map(e => <div>{e.getKey.toXml}&nbsp;{e.getValue.toXml}</div>) ++ Text(">>")

    t match {
      case o: COSObject     => obj2link(o.getObjectNumber.intValue, o.getGenerationNumber.intValue)
      case d: COSDocument   => Text(d.getHeaderString)
      case s: COSString     => Text("(" + s.getString + ")")
      case a: COSArray      => Text("[") ++ a.toList.map(o => o.toXml ++ Text(" ")).flatten ++ Text("]")
      case s: COSStream     => dict2xml(s) ++
              <div>stream</div> ++ s.getStreamTokens.map(t => token2xml(t) ++ Text(" ")).flatten ++ <div>endstream</div>
      case d: COSDictionary => dict2xml(d)
      case b: COSBoolean    => Text(b.getValue.toString)
      case n: COSName       => Text("/" + n.getName)
      case i: COSInteger    => Text(i.intValue.toString)
      case n: COSNumber     => Text(n.doubleValue.toString)
      case x: COSXRefTable  => var res = NodeSeq.Empty
                               for ((k, v) <- x.table)
                                 res ++= <div>{obj2link(k.getNumber, k.getGeneration)}={v}</div>
                               res
      case n: COSNull       => Text("null")
      case o: PDFOperator   => Text(o.getOperation)
      case o                => Text(o.toString)
    }
  }
}

case class PDFTreeNode(obj: COSBase,
                       var children: List[PDFTreeNode] = List()) {
  override def toString: String = {
    val ClassName = """.*\.COS(\w+)""".r
    val ClassName(n) = obj.getClass.getName
    n
  }

  def toXml: NodeSeq = PDFTreeNode.token2xml(obj)
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
      rootNode = docObj
      val baseChildren: List[PDFTreeNode] = List(docObj.getCatalog.getObject,
                                                 docObj.getTrailer,
                                                 COSXRefTable(docObj.getXrefTable))
      rootNode.children = if (docObj.isEncrypted)
                            docObj.getEncryptionDictionary :: baseChildren
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
        val obj: PDFTreeNode = doc.getDocument.getObjectFromPool(new COSObjectKey(num, gen)).getObject
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
  