rm -f codemirror.zip
mkdir CodeMirror
cp *.* CodeMirror
rm -f CodeMirror/codemirror.zip CodeMirror/_darcs
zip -r codemirror.zip CodeMirror
rm -rf CodeMirror
