;;;; channels.lisp

(in-package #:cl-mud)

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
