;;;; main.lisp

(in-package #:cl-mud)

(defconstant gdk-return-code #xff0d)

(defstruct app
  main-window
  main-output
  main-input)

(defvar main-app (make-app))

(defun scroll-to-end (text-view)
  (let* ((buffer (gtk-text-view-buffer text-view))
	 (insert-mark (gtk-text-buffer-get-insert buffer)))
    (gtk-text-buffer-place-cursor buffer (gtk-text-buffer-get-end-iter buffer))
    (gtk-text-view-scroll-to-mark text-view insert-mark)))

(defun setup-text-view-tags (text-view)
  (let ((tags
	 (list
	  (make-instance 'gtk-text-tag :name "0" :weight 100)
	  (make-instance 'gtk-text-tag :name "1" :weight 800)
	  (make-instance 'gtk-text-tag :name "30" :foreground "black")
	  (make-instance 'gtk-text-tag :name "31" :foreground "red")
	  (make-instance 'gtk-text-tag :name "32" :foreground "green")
	  (make-instance 'gtk-text-tag :name "33" :foreground "yellow")
	  (make-instance 'gtk-text-tag :name "34" :foreground "blue")
	  (make-instance 'gtk-text-tag :name "35" :foreground "magenta")
	  (make-instance 'gtk-text-tag :name "36" :foreground "cyan")
	  (make-instance 'gtk-text-tag :name "37" :foreground "white")
	  (make-instance 'gtk-text-tag :name "40" :background "black")
	  (make-instance 'gtk-text-tag :name "41" :background "red")
	  (make-instance 'gtk-text-tag :name "42" :background "green")
	  (make-instance 'gtk-text-tag :name "43" :background "yellow")
	  (make-instance 'gtk-text-tag :name "44" :background "blue")
	  (make-instance 'gtk-text-tag :name "45" :background "magenta")
	  (make-instance 'gtk-text-tag :name "46" :background "cyan")
	  (make-instance 'gtk-text-tag :name "47" :background "white")))
	(tag-table (gtk-text-buffer-get-tag-table (gtk-text-view-get-buffer text-view))))
    (mapcar
     (lambda (tag)
       (gtk-text-tag-table-add tag-table tag))
     tags)))

(defun make-main-window ()
  (connect host port)
  (format t "connected!~%")
  (start-reader-loop)
  (within-main-loop
    (let ((builder (make-instance 'gtk-builder)))
      (gtk-builder-add-from-file builder "main.glade")
      (loop
	 for slot in (closer-mop:class-slots (class-of main-app))
	 for slot-name = (closer-mop:slot-definition-name slot)
	 for name = (string-downcase (symbol-name slot-name))
	 for widget = (gtk-builder-get-object builder name)
	 when widget do
	   (setf (slot-value main-app slot-name) widget))
      (setup-text-view-tags (app-main-output main-app))
      (let ((quit (lambda (object)
		    (declare (ignore object))
		    (leave-gtk-main))))
	(g-signal-connect (app-main-window main-app) "destroy" quit))
      (g-signal-connect (app-main-input main-app) "key-press-event"
			(lambda (self event)
			  (declare (ignore self))
			  (if (eql (gdk-event-key-keyval event)
				   gdk-return-code)
			      (progn (send-string (gtk-entry-buffer-text
						   (gtk-entry-buffer (app-main-input main-app))))
				     (setf (gtk-entry-buffer-text
					    (gtk-entry-buffer (app-main-input main-app))) "")))))
      (gtk-widget-show-all (app-main-window main-app)))))
