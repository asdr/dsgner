(in-package :common-lisp-user)

(defpackage #:dsgner-asd
  (:use :cl :asdf))

(in-package #:dsgner-asd)

(defsystem dsgner
    :name "dsgner"
    :version "0.1"
    :serial t
    :components ((:file "defpackage")
		 (:file "dsgner"))
    :depends-on ())