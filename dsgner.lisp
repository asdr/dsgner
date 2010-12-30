(in-package :dsgner)

(defun empty-string ()
  (make-array '(0) 
              :element-type 'base-char
              :fill-pointer 0
              :adjustable t))

(defmacro with-use-tag ((tag) (&optional &rest attributes) &body body)
  (let ((strsym (gensym))
        (attsym (gensym))
        (len-attsym (gensym))
        (csym (gensym))
	(tagsym (gensym))
        (tag-name (string-downcase (string tag))))
    `(let* (,@(unless (null attributes)
		      (list `(,attsym ',attributes)
			    `(,len-attsym (length ,attsym))))
            (,strsym (empty-string)))
       (with-output-to-string (,tagsym ,strsym)
         ,@(if (not (null attributes))
               (list `(format ,tagsym "<~A" ,tag-name)
		     `(do ((,csym 0 (+ ,csym 2)))
			  ((>= ,csym ,len-attsym))
			(format ,tagsym 
				" ~A=\"~A\"" 
				(string-downcase (string (car ,attsym))) 
				(cadr ,attsym))
			(setf ,attsym (cddr ,attsym))))
	       (list `(format ,tagsym "<~A" ,tag-name)))
         
         ;;simdi body kismindaki kod icin biseyler yapmak lazim
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
  `(defmacro ,tag ((&optional &rest attribs) &body body)
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