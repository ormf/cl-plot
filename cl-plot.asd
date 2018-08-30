;;;; cl-plot.asd

(asdf:defsystem #:cl-plot
  :description "small wrapper for gnuplot"
  :author "Orm Finnendahl <orm.finnendahl@selma-hfmdk-frankfurt.de>"
  :license  "GPL Version 2.0 or later"
  :version "0.0.1"
  :serial t
  :depends-on ((:version "uiop" "3.2.0"))
  :components ((:file "package")
               (:file "cl-plot")))
