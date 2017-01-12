;;;; telnet.lisp

(in-package #:cl-mud)

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
