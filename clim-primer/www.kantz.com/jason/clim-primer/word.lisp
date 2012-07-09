(in-package clim-user)

(define-application-frame word		;name
     ()					;superclasses
  ((doc-title				;slots
    :accessor doc-title
    :initarg :doc-title))
   ;; :panes option
   (:panes
   ;; pane description
    (title				;pane name 
     :application			;pane type
     ;; pane options
     :display-function #'display-doc-title
     :initial-cursor-visibility nil)
   ;; pane description
    (document				;pane name 
     :application))			;pane type
   ;; :layouts option
   (:layouts
    (default				;name of the layout
	(vertically ()			;layout macros
	  (1/4 title)
	  (3/4 document)))))

(defmethod display-doc-title ((frame word) stream)
  (draw-text* stream "Document:" 10 15)
  (if (slot-boundp frame 'doc-title)
      (draw-text* stream (doc-title frame) 20 40)
      (draw-text* stream "Untitled" 20 40)))


;;; utilities

(defun line-height (frame pane-name)
  "Return the line height of the default text style for pane-name."
  (text-style-height *default-text-style* (get-frame-pane frame pane-name)))

(defmacro do-file ((path line-variable &key (key #'identity)) &body body)
  "Iterate over the lines of the file, binding the line to the line-variable in each iterateion"
  (let ((str (gensym))
	(var (gensym)))
    `(with-open-file (,str ,path :direction :input)
       (do ((,var (read-line ,str nil)
		  (read-line ,str nil)))
	   ((not ,var))
	 (let ((,line-variable (funcall ,key ,var)))
	   ,@body)))))

