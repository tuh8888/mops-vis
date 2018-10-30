(defsystem "mops-vis"
    :description "mops-vis: A visualization tool for MOPs using JavaScript D3"
  :version "0.0.0"
  :author "Harrison Pielke-Lombardo <harrison.pielke-lombardo@ucdenver.edu>"
  :serial t
  :components ((:file "package")
               (:file "src/common-lisp/net-vis")
               (:file "src/common-lisp/mops-vis")
               (:file "src/common-lisp/autocomplete")))
