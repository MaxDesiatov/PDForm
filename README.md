# PDForm

PDForm is all-in-one debugger/viewer/mangler/deformer for your PDF files.  Its goal is to support as many
features of PDF 1.7 as possible.  At this moment it uses Apache PDFBox for parsing and rendering, so
feature-completeness of PDForm depends on feature-completeness of PDFBox, but this will change.
The most annoying missing features will be added or reimplemented in PDForm separately, as obviously
it isn't clearly possible to integrate some Scala code in pure Java project.

## Building

If you haven't done it, setup sbt as described in http://code.google.com/p/simple-build-tool/wiki/Setup

Then do this in PDForm's root directory

    sbt update run

After that in most cases you'll see PDForm running.  If not, report this as PDForm issue at
http://github.com/explicitcall/PDForm/issues