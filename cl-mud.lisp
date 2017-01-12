;;;; cl-mud.lisp

(in-package #:cl-mud)

(defvar host "bat.org")
(defvar port 23)
(defvar io)
(defvar io-stream)

(defconstant gdk-return-code #xff0d)

(defstruct app
  main-window
  main-output
  main-input)

(defvar main-app (make-app))

(defvar reader-thread nil)

(defvar telnet-thread nil)
(defvar telnet-channel (make-instance 'bounded-channel :size 1024)) ;;receives bytes

(defvar ansi-thread nil)
(defvar ansi-channel (make-instance 'bounded-channel :size 1024)) ;;receives chars

(defvar line-splitter-thread nil)
(defvar line-splitter-channel
  (make-instance 'bounded-channel :size 1024)) ;;receives lists or chars

(defvar printer-thread nil)
(defvar printer-channel
  (make-instance 'bounded-channel :size 1024)) ;;receives lists or strings

(defun scroll-to-end (text-view)
  (let* ((buffer (gtk-text-view-buffer text-view))
	 (insert-mark (gtk-text-buffer-get-insert buffer)))
    (gtk-text-buffer-place-cursor buffer (gtk-text-buffer-get-end-iter buffer))
    (gtk-text-view-scroll-to-mark text-view insert-mark)))

(defun parse-byte (b)
  ;;(format t "~c" (code-char b))
  ;;(force-output)
  (when (not (eql b (char-code #\newline)))
    (gdk:gdk-threads-enter)
    (gtk-text-buffer-insert (gtk-text-view-buffer (app-main-output main-app))
			    (string (code-char b)))
    (scroll-to-end (app-main-output main-app))
    (gdk:gdk-threads-leave)))

(defvar telnet-commands
  '(:IAC 255 ;;interpret as command
    :GA 249  ;;go ahead
    ))

(defun start-telnet-thread ()
  "Receives bytes from network. Parses telnet related parts before passing data to ansi channel."
  (if (and telnet-thread (task-thread telnet-thread))
      (kill (task-thread telnet-thread)))
  (setf
   telnet-thread
   (pexec ()
     (start-ansi-thread)
     (loop for b = (recv telnet-channel) do
	  (cond ((eq b (getf telnet-commands :IAC))
		 (print "telnet: IAC")
		 (cond ((eql (recv telnet-channel) (getf telnet-commands :GA))
			(send ansi-channel #\newline))))
		(t 
		 (send ansi-channel (code-char b))))))))

(defun receive-ansi-colors ()
  (let (result part)
    (loop for c = (recv ansi-channel) while (not (eql c #\m)) do
	 (if (eql c #\;)
	     (progn
	       (push (concatenate 'string (reverse part)) result)
	       (setf part nil))
	     (progn
	       (push c part))))
    (push (concatenate 'string (reverse part)) result)
    (list :type :ansi-style :data (reverse result))))

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

(defun start-line-splitter-thread ()
  (if (and line-splitter-thread (task-thread line-splitter-thread))
      (kill (task-thread line-splitter-thread)))
  (setf
   line-splitter-thread
   (pexec ()
     (start-printer-thread)
     (let (line chars)
       (loop
	  for c = (recv line-splitter-channel)
	  for type = (type-of c) do
	    (cond ((eql type 'standard-char)
		   (cond ((eql c #\newline)
			  (push (reverse (concatenate 'string chars)) line)
			  (send printer-channel (reverse line))
			  (setf line nil)
			  (setf chars nil))
			 (t (push c chars))))
		  ((eql type 'cons)
		   (push (reverse (concatenate 'string chars)) line)
		   (push c line)
		   (setf chars nil))))))))

(defun start-ansi-thread ()
  (if (and ansi-thread (task-thread ansi-thread))
      (kill (task-thread ansi-thread)))
  (setf
   ansi-thread
   (pexec ()
     (start-line-splitter-thread)
     (loop for c = (recv ansi-channel) do
	  (if (eql c #\esc)
	      (if (eql (recv ansi-channel) #\[)
		  (send line-splitter-channel (receive-ansi-colors)))
	      (send line-splitter-channel c))))))

(defun start-reader-loop ()
  (if (and reader-thread (task-thread reader-thread))
      (kill (task-thread reader-thread)))
  (setf
   reader-thread
   (pexec ()
     (start-telnet-thread)
     (loop while t do (send telnet-channel (read-byte io-stream))))))

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

(defun connect (host port)
  (setf io (socket-connect host port :element-type '(unsigned-byte 8)))
  (setf io-stream (socket-stream io)))

(defun send-string (s)
  (mapcar
   (lambda (c) (write-byte (char-code c) io-stream))
   (coerce s 'list))
  (write-byte (char-code #\newline) io-stream)
  (write-byte (char-code #\return) io-stream)
  (force-output io-stream))

;; (setf tag (make-instance 'gtk-text-tag :foreground "blue" :background "black"))
;; (gtk-text-tag-table-add (gtk-text-buffer-get-tag-table buffer) tag)
;; (gtk-text-buffer-apply-tag
;;  buffer tag
;;  (gtk-text-buffer-get-iter-at-offset buffer 0)
;;  (gtk-text-buffer-get-iter-at-offset buffer 10))

(defun main ()
  (make-main-window)
  (join-gtk-main))
