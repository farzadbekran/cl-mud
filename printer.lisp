;;;; printer.lisp

(in-package #:cl-mud)

(defun start-printer-thread ()
  (if (and printer-thread (task-thread printer-thread))
      (kill (task-thread printer-thread)))
  (setf
   printer-thread
   (pexec ()
     (let (last-tags)
       (loop for line = (recv printer-channel) do
	    (let ((buffer (gtk-text-view-buffer (app-main-output main-app))))
	      (print line)
	      (gdk:gdk-threads-enter)
	      (mapcar (lambda (item)
			(cond ((stringp item)
			       (gtk-text-buffer-insert
				(gtk-text-view-buffer (app-main-output main-app))
				item)
			       (let* ((end (gtk-text-buffer-get-end-iter buffer))
				      (start (gtk-text-buffer-get-iter-at-offset
					      buffer
					      (- (length (gtk-text-buffer-text buffer))
						 (length item)))))
				 (mapcar (lambda (tag-name)
					   (if (stringp tag-name)
					       (gtk-text-buffer-apply-tag-by-name
						buffer tag-name start end)))
					 (getf last-tags :data))))
			      ((and (listp item) (eql (getf item :type) :ansi-style))
			       (setf last-tags item))))
		      line)
	      (gtk-text-buffer-insert buffer (string #\newline))
	      (scroll-to-end (app-main-output main-app))
	      (gdk:gdk-threads-leave)))))))
