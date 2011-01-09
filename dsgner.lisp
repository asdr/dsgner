(in-package :common-lisp-user)

(defpackage :dsgner
  (:use #:cl)
  (:export #:empty-string
	   #:deftag
	   #:deftags))

(in-package :dsgner)

(defun empty-string ()
  (make-array '(0) 
              :element-type 'base-char
              :fill-pointer 0
              :adjustable t))

(defmacro nif (test then &optional else)
  `(if (not ,test)
       ,then
       ,else))

(defmacro format-tag (stream tag &rest attributes)
  (let ((alen (length attributes))
	(fs "")
	(acount 0))
    (do ((i 0 (+ i 2)))
	((>= i alen))
      (setf fs (concatenate 'string 
			    fs 
			    (format nil "~A" " ~A=\"~A\""))))
    `(format ,stream ,(format nil "<~A~A" "~A" fs)
	     ,tag
	     ,@(mapcar #'(lambda(a)
			   (incf acount)
			   (if (zerop (mod acount 2))
			       a
			       (string-downcase (string a))))
		       attributes))))


;;this is used in order not to use
;;with-output-to-string macro more than once 
(defun dscanner ()
  (let ((depth 0))    
    (values 
     ;;returns depth
     (lambda ()
       depth)
     
     ;;incf depth
     (lambda()
       (incf depth))
     
     ;;decf depth
     (lambda()
       (decf depth)))))


(defmacro with-use-tag ((tag) (&rest attributes) &body body)
  (let ((strsym (gensym))
	(tagsym (gensym))
	(tag-name (string-downcase (string tag))))
    (let ((base-list 
	    (append 
	       ;;handle attributes
	       (nif (null attributes)
		    (list `(format-tag ,tagsym ,tag-name ,@attributes))
		    (list `(format-tag ,tagsym ,tag-name)))
	       
	       ;;handle BODY part
	       (cond ((null body)
		      (list `(format ,tagsym " />")))
		     
		     (t 
		      (append (list `(format ,tagsym ">"))
			      (mapcar #'(lambda (f) 
					  `(format ,tagsym "~A" ,f))
				      body)
			      (list `(format ,tagsym "</~A>" ,tag-name))))))))
      
      `(let ((,strsym (empty-string)))
	 (with-output-to-string (,tagsym ,strsym)
	   ,@base-list)
	 ,strsym))))

(defmacro deftag (tag)
  `(defmacro ,tag ((&rest attribs) &body body)
     `(with-use-tag (,',tag)
	,attribs
        ,@body)))

(defmacro deftags (&rest tags)
  `(progn
     ,@(mapcar #'(lambda(tag)
                   `(deftag ,tag))
               tags)))
