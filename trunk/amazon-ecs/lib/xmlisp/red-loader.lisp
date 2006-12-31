(dolist (fname '("package.lisp" "xml-utilities.lisp" "element-definition.lisp"
		 "xml-tag-definitions.lisp" "element-implementation.lisp"
		 "name-character-check.lisp" "xml-parser.lisp" "xml-writer.lisp"
		 "XMLisp.lisp"))
  (load (format nil "/home/red/code/lib/xmlisp/~A" fname)))
