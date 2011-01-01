(in-package #:common-lisp-user)

(defpackage #:dsgner
  (:use #:cl)
  (:export #:empty-string
	   #:deftag
	   #:deftags
	   #:with-indentation))

(in-package #:dsgner)

(defun empty-string ()
  (make-array '(0) 
              :element-type 'base-char
              :fill-pointer 0
              :adjustable t))

(defparameter *indent-enable* nil)

(let((indent-enable nil)
     (indent-count 2)
     (indent-level -1)
     (indent-string (let ((str (empty-string))
			  (ch #\Space))
		      (dotimes (i 500) ;/ 500 2 = 250 levels max
			(vector-push-extend ch str))
		      str)))  
  (defun indent-space (level)
    (format nil "~%~A" (subseq indent-string 
			       0 
			       (* level indent-count))))
  (defun indent-next-level ()
    (incf indent-level))
  (defun indent-start-over ()
    (setf indent-level -1))
  (defun indent-count ()
    indent-count)
  (defun indent-enable? ()
    indent-enable)
  (defun indent-enable ()
    (setf indent-enable t))
  (defun indent-disable ()
    (setf indent-enable nil))
  
  "Indentation Support")

(defmacro nif (test then &optional else)
  `(if (not ,test)
       ,then
       ,else))

(defun format-tag (stream tag attributes &optional (level 0))
  (let* ((indent-str (indent-space level))
	 (output (nif (zerop level)
		      (format stream "~A<~A" indent-str tag)
		      (format stream "<~A" tag))))
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
	(tag-name (string-downcase (string tag)))
	(indent-number (if (indent-enable?) 
			   (indent-next-level)
			   0)))
    `(let (,@(nif (null attributes)
		  (list `(,attsym ',attributes)))
	   (,strsym (empty-string)))

       (with-output-to-string (,tagsym ,strsym)
         ;;handle attributes
         ,(nif (null attributes)
	       `(format-tag ,tagsym ,tag-name ,attsym ,indent-number)
	       `(format-tag ,tagsym ,tag-name nil ,indent-number))
	 
	 ;;handle BODY part
	 ,@(cond ((null body)
		  (list `(format ,tagsym " />")))

		 ((and (= (length body) 1)
		       (atom (car body)))
		  (list `(format ,tagsym ">~A" ,(car body))
			`(format ,tagsym "</~A>" ,tag-name)))
				  
		 (t (append (list `(format ,tagsym ">"))
			    (mapcar #'(lambda (f) 
					`(format ,tagsym "~A" ,f))
				    body)
			    (list `(format ,tagsym "</~A>" ,tag-name))))))
       ,strsym)))

(defmacro with-indentation (&body body)
  (let ((outsym (gensym)))
    (indent-enable)
    (indent-start-over)
    `(let ((,outsym (progn
		      ,@body)))
       (indent-disable)
       ,outsym)))

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

;;define tags of HTML 4.01 / XHTML 1.0
(deftags :!DOCTYPE :a :abbr :acronym :address :applet :b :base :basefont 
	 :bdo :big :blockquote :body :br :button :caption :center :cite 
	 :code :col :colgroup :dd :del :dfn :dir :div :dl :dt :em :fieldset 
	 :font :form :frame :frameset :h1 :h2 :h3 :h4 :h5 :h6 :head :hr
	 :html :i :iframe :img :input :ins :isindex :kbd :label :legend :li 
	 :link :map :menu :meta :noframes :noscript :object :ol :optgroup
	 :option :p :param :pre :q :s :samp :script :select :small :span 
	 :strike :strong :style :sub :sup :table :tbody :td :textarea :tfoot
	 :th :thead :title :tr :tt :u :ul :var :xmp)