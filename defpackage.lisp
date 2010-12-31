(defpackage :dsgner
  (:nicknames :dsgner)
  (:use :cl)
  (:export :*indent-count*
	   :*indent-level*
	   #:indent-next-level
	   #:indent-start-over
	   #:indent-count))

(in-package :dsgner)