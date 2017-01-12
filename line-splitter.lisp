;;;; line-splitter.lisp

(in-package #:cl-mud)

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
