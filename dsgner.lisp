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

(defun format-tag (stream tag attributes)
  (let ((output (format stream "<~A" tag)))
    (when attributes
      (do ((i 0 (+ i 2))
           (att attributes (cddr att))
           (len (length attributes)))
          ((>= i len))
        (concatenate 'string 
                     output
                     (format stream
                             " ~A=\"~A\""
                             (string-downcase (string (car att)))
                             (cadr att)))))
    output))
      
(defmacro with-use-tag ((tag) (&rest attributes) &body body)
  (let ((strsym (gensym))
	(attsym (gensym))
	(tagsym (gensym))
	(tag-name (string-downcase (string tag))))
    `(let (,@(nif (null attributes)
		  (list `(,attsym ',attributes)))
	   (,strsym (empty-string)))

       (with-output-to-string (,tagsym ,strsym)
         ;;handle attributes
         ,(nif (null attributes)
	       `(format-tag ,tagsym ,tag-name ,attsym)
	       `(format-tag ,tagsym ,tag-name nil))
	 
	 ;;handle BODY part
	 ,@(cond ((null body)
		  (list `(format ,tagsym " />")))

		 (t 
		  (append (list `(format ,tagsym ">"))
			  (mapcar #'(lambda (f) 
				      `(format ,tagsym "~A" ,f))
				  body)
			  (list `(format ,tagsym "</~A>" ,tag-name))))))
       ,strsym)))

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
