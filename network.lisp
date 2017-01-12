;;;; network.lisp

(in-package #:cl-mud)

(defvar host "bat.org")
(defvar port 23)
(defvar io)
(defvar io-stream)

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

(defun start-reader-loop ()
  (if (and reader-thread (task-thread reader-thread))
      (kill (task-thread reader-thread)))
  (setf
   reader-thread
   (pexec ()
     (start-telnet-thread)
     (loop while t do (send telnet-channel (read-byte io-stream))))))
