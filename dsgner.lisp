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
  (let ((fsym (gensym))
	(tag-name (string-downcase (string tag))))
    (let ((base-list 
	   (append 
	    ;;handle attributes
	    (nif (null attributes)
		 (list `(format-tag !!tag-stream!! ,tag-name ,@attributes))
		 (list `(format-tag !!tag-stream!! ,tag-name)))
	    
	    ;;handle BODY part
	    (cond ((null body)
		   (list `(format !!tag-stream!! " />")))
		  
		  (t 
		   (append (list `(format !!tag-stream!! ">"))
			   (mapcar #'(lambda (f) 
				       `(format !!tag-stream!! "~A" ,f))
				   body)
			   (list `(format !!tag-stream!! "</~A>" ,tag-name))))))))
      
      `(flet ((,fsym (!!tag-stream!!)
		,@base-list))
	 (nif (boundp '!!empty-string!!)
	      (let ((!!empty-string!! (empty-string)))
		(with-output-to-string (!!tag-stream!! !!empty-string!!)
		  (,fsym !!tag-stream!!))
		!!empty-string!!)
	      (,fsym !!tag-stream!!))))))

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
