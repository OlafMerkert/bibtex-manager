(defsystem bibtex-manager
  :depends-on ("ol-utils" "web-utils"
               "drakma" "cxml" "closure-html"
               "css-selectors"
               "ppcre"
               "bibtex"
               "uiop" "cl-fad"
               "mcclim"
               "iterate")
  :serial t
  :components ((:file "mathscinet")
               (:file "library")
               (:file "bibtex-storage")
               (:file "manager-clim-ui")))
