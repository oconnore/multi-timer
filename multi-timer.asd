
;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *         Multi timer
;;;; *         A small library for handling many time outs with few threads
;;;; *         multi-timer.asd - The ASDF system definition
;;;; *         by Eric O'Connor
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************

(in-package :cl-user)

(defpackage multi-timer-system
    (:use :cl :asdf))

(in-package :multi-timer-system)

(defsystem multi-timer
  :name "Multi Timer"
  :author "Eric O'Connor"
  :maintainer "Eric O'Connor"
  :description "A small library for handling many time outs with few threads"
  :version "1.0.0"
  :license "MIT"
  :depends-on (bordeaux-threads queues.priority-cqueue)
  :serial t
  :components ((:file "package")
	       (:file "timer")))

;;; ===========================================================================
;;; End of file
;;; ===========================================================================