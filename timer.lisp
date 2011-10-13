
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
  empty-cv fair-cv last-time
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

;;;
;;; Condition variables
;;;

(defstruct cv
  (lock (make-lock))
  (var (make-condition-variable))
  (count 0)
  (notified 0)
  (accum nil)
  (accum-max nil))

;;; ----------------------------------------------------------------------------

(defvar *current-cv* nil
  "Bound to the current cv during predicate calls in cv-wait and cv-notify")

;;; ----------------------------------------------------------------------------

(defun cv-wait (cv &optional predicate)
  (when cv
    (let ((lock (cv-lock cv))
	  (*current-cv* cv))
      (with-lock-held (lock)
	(when (if predicate (funcall predicate) t)
	  (loop
	     (unless (plusp (cv-notified cv))
	       (incf (cv-count cv))
	       (condition-wait (cv-var cv) lock))
	     (when (plusp (cv-notified cv))
	       (decf (cv-notified cv))
	       (return-from cv-wait t))))))))

;;; ----------------------------------------------------------------------------

(defun cv-notify (cv &optional predicate)
  (when cv
    (let ((lock (cv-lock cv))
	  (*current-cv* cv))
      (with-lock-held (lock)
	(when (if predicate (funcall predicate) t)
	  (when (or (plusp (cv-count cv))
		    (cv-accum cv))
	    (setf (cv-notified cv)
		  (if (cv-accum-max cv)
		      (min (1+ (cv-notified cv))
			   (cv-accum-max cv))
		      (1+ (cv-notified cv)))))
	  (when (plusp (cv-count cv))
	    (decf (cv-count cv))
	    (condition-notify (cv-var cv))))))))

;;;
;;; Timer definitions
;;;

(defun timer-handling ()
  (flet ((maybe-invoke-event (event)
	   (when (< (timer-event-time event)
		    (get-internal-real-time))
	     (if (timer-event-thread event)
		 (interrupt-thread
		  (timer-event-thread event)
		  (timer-event-fn event))
		 (make-thread (timer-event-fn event)))
	     t)))
    (let ((*in-scope?* nil))
      (loop
	 (catch 'interrupt
	   ;; When constructing a timer thread, the constructor waits on the cv
	   ;; one time, to give this function time to set up
	   (cond ((plusp (qsize (timer-queue *timer*)))
		  (let (event)
		    (unwind-protect
			 (progn (setf event (qtop (timer-queue *timer*) nil))
				;; We only sleep if the timer is not expired
				(when (and event (timer-event-p event)
					   (> (timer-event-time event)
					      (get-internal-real-time)))
				  (let ((*in-scope?* t))
				    (declare (ignorable *in-scope?*))
				    ;; Sleep here, and allow interrupts
				    (let ((secs (i2secs
						 (- (timer-event-time event)
						    (get-internal-real-time)))))
				      (cv-notify (timer-fair-cv *timer*)
						 (lambda ()
						   (setf (timer-last-time *timer*)
							 (get-internal-real-time))
						   t))
				      (when (plusp secs) (sleep secs))))))
		      ;; Attempt to invoke the active timer
		      (when (and event (maybe-invoke-event event))
			(qpop (timer-queue *timer*))))))
		 (t
		  (cv-notify (timer-fair-cv *timer*)
			     (lambda ()
			       (setf (timer-last-time *timer*)
				     (get-internal-real-time))
			       t))
		  (cv-wait (timer-empty-cv *timer*)))))))))

;;; ----------------------------------------------------------------------------

(defun ensure-timer-thread (&optional force)
  (when (or force (not *timer*))
    (setf *timer*
	  (make-timer 
	   :empty-cv (make-cv)
	   :fair-cv (make-cv 
		     :accum t
		     :accum-max 1
		     :notified 1)
	   :last-time 0
	   :queue (make-queue :type 'priority-cqueue
			      :compare (lambda (x y)
					 (< (timer-event-time x)
					    (timer-event-time y)))))
	  (timer-thread *timer*) (make-thread #'timer-handling))
    t))

;;; ----------------------------------------------------------------------------

(defun %int-thrower ()
  (when *in-scope?*
    (throw 'interrupt t)))

;;; ----------------------------------------------------------------------------

(defun set-timer (fn time &optional new-thread)
  (ensure-timer-thread)
  (cv-wait (timer-fair-cv *timer*)
	   (lambda ()
	     (and (> (get-internal-real-time)
		     (+ (timer-last-time *timer*)
			(secs2i .0001)))
		  (zerop (cv-count *current-cv*)))))
  (let ((event (make-timer-event
		:fn fn
		:thread (unless new-thread (current-thread))
		:time (+ (secs2i time)
			 (get-internal-real-time)))))
    (multiple-value-bind (ign node)
	(qpush (timer-queue *timer*) event)
      (declare (ignore ign))
      (cv-notify (timer-empty-cv *timer*))
      (interrupt-thread (timer-thread *timer*) #'%int-thrower)
      (values event node))))

;;; ----------------------------------------------------------------------------

(defun remove-timer (timer)
  (ensure-timer-thread)
  (let ((node
	 (if (queue-node-p timer)
	     timer
	     (block search
	       (queue-find (timer-queue *timer*)
			   (lambda (x)
			     (when (eq x timer)
			       (return-from search
				 *current-queue-node*))))))))
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

(defmacro with-mtimeout ((time-form &rest timeout-forms) &rest try-forms)
  (let ((block (gensym "multi-timer-block"))
	(timeout (gensym "multi-timer-timeout"))
	(node (gensym "multi-timer-node")))
    `(block ,block
       (catch ',timeout
	 (let (,node)
	   (unwind-protect
		(progn
		  (setf ,node
			(nth-value 1
				   (set-timer (lambda ()
						(throw ',timeout t))
					      ,time-form)))
		  (return-from ,block
		    (progn ,@try-forms)))
	     (when ,node
	       (remove-timer ,node)))))
       ,@timeout-forms)))
      

;;; ===========================================================================
;;; End of file
;;; ===========================================================================