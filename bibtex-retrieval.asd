(defsystem bibtex-retrieval
  :depends-on (ol-utils web-utils
               drakma cxml closure-html
               css-selectors
               bibtex)
  :serial t
  :components ((:file "mathscinet")))
