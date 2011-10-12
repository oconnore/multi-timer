
;;;; **************************************************************************
;;;; **************************************************************************
;;;; *
;;;; *         Multi timer
;;;; *         A small library for handling many time outs with one thread
;;;; *         Timer - The multi-timer functions
;;;; *         by Eric O'Connor
;;;; *
;;;; **************************************************************************
;;;; **************************************************************************

(in-package :multi-timer)

;;;
;;; Variables
;;;

(defvar *timer* nil)
(defvar *in-scope?* nil)

;;;
;;; Data types
;;;

(defstruct timer
  thread
  cv-lock
  cv
  cv-wait
  queue)

(defmethod print-object ((o timer) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~A"
	    (when (timer-queue o)
	      (qsize (timer-queue o))))))

(defstruct timer-event
  time thread fn)

(defmethod print-object ((o timer-event) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~A [~A]" (timer-event-fn o)
	    (when (timer-event-time o)
	      (i2secs (- (timer-event-time o)
			 (get-internal-real-time)))))))

;;;
;;; Quick Utility Functions
;;;

(defun i2secs (x)
  (/ (coerce x 'double-float)
     #.(coerce internal-time-units-per-second 'double-float)))

;;; ----------------------------------------------------------------------------


(defun secs2i (x)
  (values (floor (* (coerce x 'double-float)
		    #.(coerce internal-time-units-per-second 'double-float)))))

;;; ----------------------------------------------------------------------------

(defun timer-wait (&optional (tm *timer*))
  (when tm
    (let ((cv (timer-cv tm))
	  (cvl (timer-cv-lock tm)))
      (with-lock-held (cvl)
	(incf (timer-cv-wait tm))
	(when (plusp (timer-cv-wait tm))
	  (condition-wait cv cvl))))))

;;; ----------------------------------------------------------------------------

(defun timer-notify (&optional (tm *timer*))
  (when tm
    (let ((cv (timer-cv tm))
	  (cvl (timer-cv-lock tm)))
      (with-lock-held (cvl)
	(unwind-protect
	     (decf (timer-cv-wait tm))
	  (unless (minusp (timer-cv-wait tm))
	    (condition-notify cv)))))))

;;;
;;; Timer definitions
;;;
(defun timer-handling ()
  (declare (optimize (debug 3)))
  (flet ((maybe-invoke-event (event)
	   (when (< (timer-event-time event)
		    (get-internal-real-time))
	     (if (timer-event-thread event)
		 (interrupt-thread
		  (timer-event-thread event)
		  (timer-event-fn event))
		 (make-thread (timer-event-fn event)))
	     t)))
    (let ((*in-scope?* nil)
	  (first t))
      (loop
	 (catch 'interrupt
	   ;; When constructing a timer thread, the constructor waits on the cv
	   ;; one time, to give this function time to set up
	   (when first (timer-notify) (setf first nil))
	   (cond ((plusp (qsize (timer-queue *timer*)))
		  (let (event)
		    (unwind-protect
			 (progn (setf event (qtop (timer-queue *timer*) nil))
				;; We only sleep if the timer is not expired
				(when (> (timer-event-time event)
					 (get-internal-real-time))
				  (let ((*in-scope?* t))
				    (declare (ignorable *in-scope?*))
				    ;; Sleep here, and allow timer functions
				    ;; to acquire the lock (and therefore invoke
				    ;; a timer-interrupt).
				    (let ((secs (i2secs
						 (- (timer-event-time event)
						    (get-internal-real-time)))))
				      (when (plusp secs) (sleep secs))))))
		      ;; If an interrupt occurred, attempt to invoke the active timer
		      (when (and event (maybe-invoke-event event))
			(qpop (timer-queue *timer*))))))
		 (t (timer-wait))))))))

;;; ----------------------------------------------------------------------------

(defun ensure-timer-thread (&optional force)
  (when (or force (not *timer*))
    (setf *timer*
	  (make-timer 
	   :cv-lock (make-lock)
	   :cv (make-condition-variable)
	   :cv-wait 0
	   :queue (make-queue :type 'priority-cqueue
			      :compare (lambda (x y)
					 (< (timer-event-time x)
					    (timer-event-time y)))))
	  (timer-thread *timer*) (make-thread #'timer-handling))
    ;; Wait for the new thread to initiate
    (timer-wait)
    t))

;;; ----------------------------------------------------------------------------

(defun %int-thrower ()
  (when *in-scope?*
    (throw 'interrupt t)))

;;; ----------------------------------------------------------------------------

(defun set-timer (fn time &optional new-thread)
  (ensure-timer-thread)
  (let ((event (make-timer-event
		:fn fn
		:thread (unless new-thread (current-thread))
		:time (+ (secs2i time)
			 (get-internal-real-time)))))
    (qpush (timer-queue *timer*) event)
    (timer-notify)
    (interrupt-thread (timer-thread *timer*) #'%int-thrower)
    event))

;;; ----------------------------------------------------------------------------

(defun remove-timer (timer)
  (ensure-timer-thread)
  (let ((node
	 (block search
	   (queue-find (timer-queue *timer*)
		       (lambda (x)
			 (when (eq x timer)
			   (return-from search *current-queue-node*)))))))
      (when node
	(queue-delete (timer-queue *timer*) node)
	timer)))

;;; ----------------------------------------------------------------------------

(defun map-timers (fn)
  (ensure-timer-thread)
  (map-queue fn (timer-queue *timer*)))

;;; ----------------------------------------------------------------------------

(defun clear-timers ()
  (when (and *timer* (timer-thread *timer*)
	     (thread-alive-p (timer-thread *timer*)))
    (destroy-thread (timer-thread *timer*)))
  (ensure-timer-thread t))

;;; ----------------------------------------------------------------------------

;;(defmacro with-mtimeout ((time-form &rest timeout-forms) try-form)
;;  (

;;; ===========================================================================
;;; End of file
;;; ===========================================================================