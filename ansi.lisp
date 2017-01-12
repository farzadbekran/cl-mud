;;;; ansi.lisp

(in-package #:cl-mud)

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
