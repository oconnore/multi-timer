
Multi Timer (for Lisp!)
=======================

This is a small timer library for Common Lisp. It's goals are to provide efficient and accurate timers across multiple Common Lisp platforms. While cross compatible timers already exist, they are typically heavyweight, spawning a thread per timer [1]. This library eliminates that overhead, by using a single worker thread to manage all timers for an entire process.

[1]: SBCL has [awesome timers](http://www.sbcl.org/manual/index.html#Timers) that will likely perform better if you only have to deploy to SBCL.

Bug reports/fixes welcome: <Eric O'Connor> oconnore@gmail.com

Application Programming Interface
---------------------------------

Package: multi-timer

### General functions

* **set-timer** (fn time &optional new-thread)

    * *fn* A function of no arguments that is called when the timer expires
    * *time* A positive real number representing the seconds before the timer expires
    * *new-thread* When t, run the *fn* in a new thread

    Returns an event, which may be passed to #'remove-timer to cancel the timer.

* **remove-timer**

    * *timer* A timer to cancel

* **map-timers** (fn)

    * *fn* A function to map over all active timers

* **clear-timers**

    Clears all active timers.

* **with-mtimeout** ((time-form &rest timeout-forms) &rest try-forms)

    Attempts to execute _try-forms_ for _time-form_ seconds, but cancels the execution and runs _timeout-forms_ if the timer expires.

* **timer-event-p**

    Predicate to test for a timer-event object