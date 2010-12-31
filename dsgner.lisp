(in-package :dsgner)

(defun empty-string ()
  (make-array '(0) 
              :element-type 'base-char
              :fill-pointer 0
              :adjustable t))


(defparameter *indent-string* (let ((str (empty-string))
				    (ch #\Space))
				(dotimes (i 500) ;/ 500 2 = 250 levels max
				  (vector-push-extend ch str))
				str))

(defparameter *indent-count* 2)
(defparameter *indent-level* 0)

(defun indent-space (level)
  (format nil "~%~A" (subseq *indent-string* 0 (* level *indent-count*))))

(defun indent-next-level ()
  (incf *indent-level*)
  (1- *indent-level*))

(defun indent-start-over ()
  (setf *indent-level* 0))

(defun indent-count ()
  *indent-count*)

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


(defun test1 ()
  (:div (:class "x-tree-root-node-ct")
	(:li (:class "x-tree-root-node")
	     (:div ())
	     (:ul ()
		  (:li ()
		       (:img (:src "icons/xxx.png")))))))

(defun test2 ()
  (:html (:xmlns "http://www.w3.org/1999/xhtml"
		 :xml\:lang "en")
	 (:head ()
		(:meta (:http-equiv "Content-Type"
				    :content "text/html;charset=utf-8"))
		(:title ()
			"Web Page")
		(:link (:type "text/css"
			      :rel "stylesheet"
			      :href "/general.css")))
	 (:body ()
		(:div (:id "header")
		      (:p ()
			  (:h1 ()
			       "HEADER..."))))))