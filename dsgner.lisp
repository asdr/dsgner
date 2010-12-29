
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

(defun empty-string ()
  (make-array '(0) 
              :element-type 'base-char
              :fill-pointer 0
              :adjustable t))

(defparameter *tag-string* (empty-string))
 
(defun clean-output-buffer ()
  (setf *tag-string* (empty-string)))


(defmacro with-use-tag ((tag) (&optional &rest attributes) &body body)
  (let ((strsym (gensym))
        (attsym (gensym))
        (len-attsym (gensym))
        (csym (gensym))
        (bodysym (gensym))
        (tag-name (string-downcase (string tag))))
    `(let* (,@(unless (null attributes)
               (list
                `(,attsym ',attributes)
                `(,len-attsym (length ,attsym))))
            (,strsym *tag-string*);variable capture
            (,bodysym ',body))
       (with-output-to-string (s ,strsym)
         ,@(if (not (null attributes))
               (list
                `(format s "<~A" ,tag-name)
                `(do ((,csym 0 (+ ,csym 2)))
                     ((>= ,csym ,len-attsym))
                   (format s " ~A=\"~A\"" (string-downcase (string (car ,attsym))) (cadr ,attsym))
                   (setf ,attsym (cddr ,attsym))))
               (list 
                     `(format s "<~A" ,tag-name)))
         
         ;;simdi body kismindaki kod icin biseyler yapmak lazim
         (cond ((null ,bodysym)
                (format s " />"))
           ((and (= (length ,bodysym) 1)
                 (atom (car ,bodysym)))
            (format s ">~A" (car ,bodysym))
            (format s "</~A>" ,tag-name))
           (t (format s ">") 
              ,@body
              (format s "</~A>" ,tag-name)))))))

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
(deftags :!DOCTYPE :a :abbr :acronym :address :applet :b :base :basefont :bdo :big :blockquote :body :br :button :caption :center
  :cite :code :col :colgroup :dd :del :dfn :dir :div :dl :dt :em :fieldset :font :form :frame :frameset :h1 :h2 :h3 :h4 :h5 :h6 :head :hr
  :html :i :iframe :img :input :ins :isindex :kbd :label :legend :li :link :map :menu :meta :noframes :noscript :object :ol :optgroup
  :option :p :param :pre :q :s :samp :script :select :small :span :strike :strong :style :sub :sup :table :tbody :td :textarea :tfoot
  :th :thead :title :tr :tt :u :ul :var :xmp)


(defun test1 ()
  (let ((*tag-string* (empty-string)))
    (:div (:class "x-tree-root-node-ct")
      (:li (:class "x-tree-root-node")
        (:div ())
        (:ul ()
          (:li ()
            (:img (:src "icons/xxx.png"))))))
    (princ *tag-string*)))