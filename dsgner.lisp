
(defpackage #:dsgner-asd
  (:use :cl :asdf))

(in-package #:dsgner-asd)

(defsystem dsgner
    :name "dsgner"
    :version "0.1"
    :depends-on ())

(defpackage :dsgner
  (:nicknames :dsgner)
  (:use :cl)
  (:export 
   #:clean-output-buffer
   #:html
   #:head
   #:title
   #:body))

(in-package :dsgner)

(defparameter *tag-string* (make-array '(0) 
				       :element-type 'base-char
				       :fill-pointer 0
				       :adjustable t))
(defun clean-output-buffer ()
  (setf *tag-string* (make-array '(0) 
				 :element-type 'base-char
				 :fill-pointer 0
				 :adjustable t)))

(defmacro html (&rest body)
  `(with-use-tag (html) 
       ()
     ,@body))

(defmacro head (&rest body)
  `(with-use-tag (head) 
       ()
     ,@body))

(defmacro title (title)
  `(with-use-tag (title) 
       ()
     ,title))

(defmacro body ((&optional &rest attribs) &body body) 
  `(with-use-tag (body)
     ,attribs
     ,@body))



(defmacro with-use-tag ((tag) (&optional &rest attributes) &body body)
  (let ((strsym (gensym))
	(attsym (gensym))
	(len-attsym (gensym))
	(csym (gensym))
	(bodysym (gensym)))
    `(let* ((,attsym ',attributes)
	    (,len-attsym (length ,attsym))
	    (,strsym *tag-string*);variable capture
	    (,bodysym ',body))
       (with-output-to-string (s ,strsym)
	 (if (not (null ,attsym))
	     (progn
	       (format s "<~A" ',tag)
	       (do ((,csym 0 (+ ,csym 2)))
		   ((>= ,csym ,len-attsym))
		 (format s " ~A=\"~A\"" (first ,attsym) (second ,attsym))
		 (setf ,attsym (cddr ,attsym))))
	     (format s "<~A" ',tag))
	 
	 ;;simdi body kismindaki kod icin biseyler yapmak lazim
	 (cond ((null ,bodysym)
		(format s " />"))
	       ((and (= (length ,bodysym) 1)
		     (atom (car ,bodysym)))
		(format s ">~A" (car ,bodysym))
		(format s "</~A>" ',tag))
	       (t (format s ">") 
		  ,@body
		  (format s "</~A>" ',tag)))))))

(defun test1 ()
  (princ 
   (let ((*tag-string* (make-array '(0) 
				   :element-type 'base-char
				   :fill-pointer 0
				   :adjustable t)))
     (html
      (head
       (title "ASDR"))
      (body (:color "Green"
	     :onload "javascript: alert('asdr!');")))
     *tag-string*)))