(defpackage ulf-lib
  (:documentation "This is just an empty package to ensure that Eta will still compile when not being used in responsive mode
                  (i.e. with no dependencies for response generation).")
  (:use :common-lisp)
  (:export :apply-sub-macro)
)
;;(in-package ulf-lib)
;;(defconstant +load-path+ (system-relative-pathname 'epilog ""))
