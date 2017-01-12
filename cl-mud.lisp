;;;; cl-mud.lisp

(in-package #:cl-mud)

(defun main ()
  (make-main-window)
  (join-gtk-main))
